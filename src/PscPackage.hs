{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module PscPackage where

import qualified Control.Foldl as Foldl
import           Control.Concurrent.Async (forConcurrently_, mapConcurrently)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Error.Util (note)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (fieldLabelModifier)
import           Data.Aeson.Encode.Pretty
import           Data.Foldable (fold, foldMap, traverse_, for_)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Graph as G
import           Data.List (maximumBy)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.IORef as IORef
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Ord (comparing)
import qualified Data.Set as Set
import           Data.Text (pack)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Read as TR
import           Data.Traversable (for)
import           Data.Version (Version(..), parseVersion, showVersion)
import qualified Filesystem.Path.CurrentOS as Path
import           GHC.Generics (Generic)
import           System.FilePath.Glob (glob)
import qualified System.Process as Process
import qualified Text.ParserCombinators.ReadP as Read
import           Turtle hiding (arg, fold, s, x)
import qualified Turtle
import           Types (PackageName(..), mkPackageName, runPackageName, untitledPackageName, preludePackageName)
import Filesystem.Path.CurrentOS (encodeString)
import qualified Language.PureScript as P
import qualified Language.PureScript.Ide.Imports as PIDE

import qualified Package as Package
import qualified Dhall as Dhall

echoT :: Text -> IO ()
echoT = Turtle.printf (Turtle.s % "\n")

exitWithErr :: Text -> IO a
exitWithErr errText = errT errText >> exit (ExitFailure 1)
  where errT = traverse Turtle.err . textToLines

packageFile :: Path.FilePath
packageFile = "psc-package.json"

localPackageSet :: Path.FilePath
localPackageSet = "packages.json"

data PackageConfig = PackageConfig
  { name    :: PackageName
  , depends :: [PackageName]
  , set     :: Text
  , source  :: Text
  } deriving (Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

pathToTextUnsafe :: Turtle.FilePath -> Text
pathToTextUnsafe = either (error "Path.toText failed") id . Path.toText

shellToIOText :: Turtle.Shell Line -> IO [Text]
shellToIOText shellLines = Turtle.fold (fmap lineToText shellLines) Foldl.list

readPackageFile :: IO PackageConfig
readPackageFile = do
  exists <- testfile packageFile
  unless exists $ exitWithErr "psc-package.json does not exist. Maybe you need to run psc-package init?"
  mpkg <- Aeson.eitherDecodeStrict . encodeUtf8 <$> readTextFile packageFile
  case mpkg of
    Left errors -> exitWithErr $ "Unable to parse psc-package.json: " <> T.pack errors
    Right pkg -> return pkg

packageConfigToJSON :: PackageConfig -> Text
packageConfigToJSON =
    TL.toStrict
    . TB.toLazyText
    . encodePrettyToTextBuilder' config
  where
    config = defConfig
               { confCompare =
                   keyOrder [ "name"
                            , "set"
                            , "source"
                            , "depends"
                            ]
               , confIndent = Spaces 2
               , confTrailingNewline = True
               }

packageSetToJSON :: PackageSet -> Text
packageSetToJSON =
    TL.toStrict
    . TB.toLazyText
    . encodePrettyToTextBuilder' config
  where
    config = defConfig
               { confCompare = compare
               , confIndent = Spaces 2
               , confTrailingNewline = True
               }

writePackageFile :: PackageConfig -> IO ()
writePackageFile =
  writeTextFile packageFile
  . packageConfigToJSON

data PackageInfo = PackageInfo
  { repo         :: Text
  , version      :: Text
  , dependencies :: [PackageName]
  } deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

type PackageSet = Map.Map PackageName PackageInfo

cloneShallow
  :: Text
  -- ^ repo
  -> Text
  -- ^ branch/tag
  -> Turtle.FilePath
  -- ^ target directory
  -> IO ExitCode
cloneShallow from ref into =
  proc "git"
       [ "clone"
       , "-q"
       , "-c", "advice.detachedHead=false"
       , "--depth", "1"
       , "-b", ref
       , from
       , pathToTextUnsafe into
       ] empty .||. exit (ExitFailure 1)

listRemoteTags
  :: Text
  -- ^ repo
  -> Turtle.Shell Text
listRemoteTags from = let gitProc = inproc "git"
                                    [ "ls-remote"
                                    , "-q"
                                    , "-t"
                                    , from
                                    ] empty
                      in lineToText <$> gitProc

getPackageSet :: PackageConfig -> IO ()
getPackageSet PackageConfig{ source, set } = do
  let pkgDir = ".psc-package" </> fromText set </> ".set"
  exists <- testdir pkgDir
  unless exists . void $ cloneShallow source set pkgDir

readPackageSet :: PackageConfig -> IO PackageSet
readPackageSet PackageConfig{ set } = do
  let dbFile = ".psc-package" </> fromText set </> ".set" </> "packages.json"
  handleReadPackageSet dbFile

handleReadPackageSet :: Path.FilePath -> IO PackageSet
handleReadPackageSet dbFile = do
  exists <- testfile dbFile
  unless exists $ exitWithErr $ format (fp%" does not exist") dbFile
  mdb <- Aeson.eitherDecodeStrict . encodeUtf8 <$> readTextFile dbFile
  case mdb of
    Left errors -> exitWithErr $ "Unable to parse packages.json: " <> T.pack errors
    Right db -> return db

writePackageSet :: PackageConfig -> PackageSet -> IO ()
writePackageSet PackageConfig{ set } =
  let dbFile = ".psc-package" </> fromText set </> ".set" </> "packages.json"
  in writeTextFile dbFile . packageSetToJSON

readLocalPackageSet :: IO PackageSet
readLocalPackageSet = handleReadPackageSet localPackageSet

writeLocalPackageSet :: PackageSet -> IO ()
writeLocalPackageSet = writeTextFile localPackageSet . packageSetToJSON

performInstall :: Text -> PackageName -> PackageInfo -> IO Turtle.FilePath
performInstall set pkgName PackageInfo{ repo, version } = do
  let pkgDir = ".psc-package" </> fromText set </> fromText (runPackageName pkgName) </> fromText version
  exists <- testdir pkgDir
  unless exists . void $ do
    echoT ("Installing " <> runPackageName pkgName)
    cloneShallow repo version pkgDir
  pure pkgDir

getReverseDeps  :: PackageSet -> PackageName -> IO [(PackageName, PackageInfo)]
getReverseDeps = getReverseDeps' Set.empty
  where
    getReverseDeps' seen db dep = List.nub <$> foldMap (go seen db dep) (Map.toList db)
    go seen db dep pair@(packageName, PackageInfo {dependencies})
      | packageName `Set.member` seen =
          exitWithErr ("Cycle in package dependencies at package " <> runPackageName packageName)
      | otherwise =
        case List.find (== dep) dependencies of
          Nothing -> return mempty
          Just _ -> do
            innerDeps <- getReverseDeps' (Set.insert packageName seen) db packageName
            return $ pair : innerDeps

getTransitiveDeps :: PackageSet -> [PackageName] -> IO [(PackageName, PackageInfo)]
getTransitiveDeps db deps =
    Map.toList . fold <$> traverse (go Set.empty) deps
  where
    go seen pkg
      | pkg `Set.member` seen =
          exitWithErr ("Cycle in package dependencies at package " <> runPackageName pkg)
      | otherwise =
        case Map.lookup pkg db of
          Nothing ->
            exitWithErr ("Package " <> runPackageName pkg <> " does not exist in package set")
          Just info@PackageInfo{ dependencies } -> do
            m <- fold <$> traverse (go (Set.insert pkg seen)) dependencies
            return (Map.insert pkg info m)

installImpl :: PackageConfig -> IO ()
installImpl config@PackageConfig{ depends } = do
  getPackageSet config
  db <- readPackageSet config
  trans <- getTransitiveDeps db depends
  echoT ("Installing " <> pack (show (length trans)) <> " packages...")
  forConcurrently_ trans . uncurry $ performInstall $ set config

getPureScriptVersion :: IO Version
getPureScriptVersion = do
  let pursProc = inproc "purs" [ "--version" ] empty
  outputLines <- shellToIOText pursProc
  case outputLines of
    [onlyLine]
      | results@(_ : _) <- Read.readP_to_S parseVersion (T.unpack onlyLine) ->
           pure (fst (maximumBy (comparing (length . versionBranch . fst)) results))
      | otherwise -> exitWithErr "Unable to parse output of purs --version"
    _ -> exitWithErr "Unexpected output from purs --version"

initialize :: Maybe (Text, Maybe Text) -> IO ()
initialize setAndSource = do
    exists <- testfile "psc-package.json"
    when exists $ exitWithErr "psc-package.json already exists"
    echoT "Initializing new project in current directory"
    pkgName <- packageNameFromPWD . pathToTextUnsafe . Path.filename <$> pwd
    pkg <- case setAndSource of
      Nothing -> do
        pursVersion <- getPureScriptVersion
        echoT ("Using the default package set for PureScript compiler version " <>
          fromString (showVersion pursVersion))
        echoT "(Use --source / --set to override this behavior)"
        pure PackageConfig { name    = pkgName
                           , depends = [ preludePackageName ]
                           , source  = "https://github.com/purescript/package-sets.git"
                           , set     = "psc-" <> pack (showVersion pursVersion)
                           }
      Just (set, source) ->
        pure PackageConfig { name    = pkgName
                           , depends = [ preludePackageName ]
                           , source  = fromMaybe "https://github.com/purescript/package-sets.git" source
                           , set
                           }

    writePackageFile pkg
    installImpl pkg
  where
    packageNameFromPWD =
      either (const untitledPackageName) id . mkPackageName

install :: Maybe String -> IO ()
install pkgName' = do
  pkg <- readPackageFile
  case pkgName' of
    Nothing -> do
      installImpl pkg
      echoT "Install complete"
    Just str -> do
      pkgName <- packageNameFromString str
      let pkg' = pkg { depends = List.nub (pkgName : depends pkg) }
      updateAndWritePackageFile pkg'

uninstall :: String -> IO ()
uninstall pkgName' = do
  pkg <- readPackageFile
  pkgName <- packageNameFromString pkgName'
  let pkg' = pkg { depends = filter (/= pkgName) $ depends pkg }
  updateAndWritePackageFile pkg'

updateAndWritePackageFile :: PackageConfig -> IO ()
updateAndWritePackageFile pkg = do
  installImpl pkg
  writePackageFile pkg
  echoT "psc-package.json file was updated"

packageNameFromString :: String -> IO PackageName
packageNameFromString str =
  case mkPackageName (pack str) of
    Right pkgName ->
      pure pkgName
    Left _ -> exitWithErr $ "Invalid package name: " <> pack (show str)

listDependencies :: IO ()
listDependencies = do
  pkg@PackageConfig{ depends } <- readPackageFile
  db <- readPackageSet pkg
  trans <- getTransitiveDeps db depends
  traverse_ (echoT . runPackageName . fst) trans

listPackages :: Bool -> IO ()
listPackages sorted = do
  pkg <- readPackageFile
  db <- readPackageSet pkg
  if sorted
    then traverse_ echoT (fmt <$> inOrder (Map.assocs db))
    else traverse_ echoT (fmt <$> Map.assocs db)
  where
  fmt :: (PackageName, PackageInfo) -> Text
  fmt (name, PackageInfo{ version, repo }) =
    runPackageName name <> " (" <> version <> ", " <> repo <> ")"

  inOrder xs = fromNode . fromVertex <$> vs where
    (gr, fromVertex) =
      G.graphFromEdges' [ (pkg, name, dependencies pkg)
                        | (name, pkg) <- xs
                        ]
    vs = G.topSort (G.transposeG gr)
    fromNode (pkg, name, _) = (name, pkg)

getSourcePaths :: PackageConfig -> PackageSet -> [PackageName] -> IO [Turtle.FilePath]
getSourcePaths PackageConfig{..} db pkgNames = do
  trans <- getTransitiveDeps db pkgNames
  let paths = [ ".psc-package"
                </> fromText set
                </> fromText (runPackageName pkgName)
                </> fromText version
                </> "src" </> "**" </> "*.purs"
              | (pkgName, PackageInfo{ version }) <- trans
              ]
  return paths

getPaths :: IO [Turtle.FilePath]
getPaths = do
  pkg@PackageConfig{..} <- readPackageFile
  db <- readPackageSet pkg
  getSourcePaths pkg db depends

listSourcePaths :: IO ()
listSourcePaths = do
  paths <- getPaths
  traverse_ (echoT . pathToTextUnsafe) paths

-- | Helper for calling through to @purs@
--
-- Extra args will be appended to the options
exec :: [String] -> Bool -> [String] -> IO ()
exec execNames onlyDeps passthroughOptions = do
  pkg <- readPackageFile
  installImpl pkg

  paths <- getPaths
  let cmdParts = tail execNames
      srcParts = [ "src" </> "**" </> "*.purs" | not onlyDeps ]
  exit
    =<< Process.waitForProcess
    =<< Process.runProcess
          (head execNames)
          (cmdParts <> passthroughOptions
                    <> map Path.encodeString (srcParts <> paths))
          Nothing -- no special path to the working dir
          Nothing -- no env vars
          Nothing -- use existing stdin
          Nothing -- use existing stdout
          Nothing -- use existing stderr

checkForUpdates :: Bool -> Bool -> IO ()
checkForUpdates applyMinorUpdates applyMajorUpdates = do
    pkg <- readPackageFile
    db <- readPackageSet pkg

    echoT ("Checking " <> pack (show (Map.size db)) <> " packages for updates.")
    echoT "Warning: this could take some time!"

    newDb <- Map.fromList <$> for (Map.toList db) (\(name, p@PackageInfo{ repo, version }) -> do
      echoT ("Checking package " <> runPackageName name)
      tagLines <- Turtle.fold (listRemoteTags repo) Foldl.list
      let tags = mapMaybe parseTag tagLines
      newVersion <- case parsePackageVersion version of
        Just parts ->
          let applyMinor =
                case filter (isMinorReleaseFrom parts) tags of
                  [] -> pure version
                  minorReleases -> do
                    echoT "New minor release available"
                    if applyMinorUpdates
                      then do
                        let latestMinorRelease = maximum minorReleases
                        pure ("v" <> T.intercalate "." (map (pack . show) latestMinorRelease))
                      else pure version
              applyMajor =
                case filter (isMajorReleaseFrom parts) tags of
                  [] -> applyMinor
                  newReleases -> do
                    echoT "New major release available"
                    if applyMajorUpdates
                      then do
                        let latestRelease = maximum newReleases
                        pure ("v" <> T.intercalate "." (map (pack . show) latestRelease))
                      else applyMinor
          in applyMajor
        _ -> do
          echoT "Unable to parse version string"
          pure version
      pure (name, p { version = newVersion }))

    when (applyMinorUpdates || applyMajorUpdates)
      (writePackageSet pkg newDb)
  where
    parseTag :: Text -> Maybe [Int]
    parseTag line =
      case T.splitOn "\t" line of
        [_sha, ref] ->
          case T.stripPrefix "refs/tags/" ref of
            Just tag ->
              case parsePackageVersion tag of
                Just parts -> pure parts
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing

    parsePackageVersion :: Text -> Maybe [Int]
    parsePackageVersion ref =
      case T.stripPrefix "v" ref of
        Just tag ->
          traverse parseDecimal (T.splitOn "." tag)
        _ -> Nothing

    parseDecimal :: Text -> Maybe Int
    parseDecimal s =
      case TR.decimal s of
        Right (n, "") -> Just n
        _ -> Nothing

    isMajorReleaseFrom :: [Int] -> [Int] -> Bool
    isMajorReleaseFrom (0 : xs) (0 : ys) = isMajorReleaseFrom xs ys
    isMajorReleaseFrom (x : _)  (y : _)  = y > x
    isMajorReleaseFrom _        _        = False

    isMinorReleaseFrom :: [Int] -> [Int] -> Bool
    isMinorReleaseFrom (0 : xs) (0 : ys) = isMinorReleaseFrom xs ys
    isMinorReleaseFrom (x : xs) (y : ys) = y == x && ys > xs
    isMinorReleaseFrom _        _        = False

data VerifyArgs a = Package a | VerifyAll (Maybe a) deriving (Functor, Foldable, Traversable)

verify :: VerifyArgs Text -> IO ()
verify arg = do
  pkg <- readPackageFile
  db  <- readPackageSet pkg
  case traverse mkPackageName arg of
    Left pnError -> echoT . pack $ "Error while parsing arguments to verify: " <> show pnError
    Right (Package pName) -> case Map.lookup pName db of
      Nothing -> echoT . pack $ "No packages found with the name " <> show (runPackageName pName)
      Just _  -> do
        reverseDeps <- map fst <$> getReverseDeps db pName
        let packages = pure pName <> reverseDeps
        verifyPackages packages db pkg

    Right (VerifyAll pName) -> verifyPackages packages db pkg
      where
        packages = Map.keys $ maybe db pFilter pName
        pFilter name = Map.filterWithKey (\k _ -> runPackageName k >= runPackageName name) db

  where
    verifyPackages :: [PackageName] -> PackageSet -> PackageConfig -> IO ()
    verifyPackages names db pkg = do
      echoT $ "Verifying " <> pack (show $ length names) <> " packages."
      echoT "Warning: this could take some time!"
      traverse_ (verifyPackage db pkg) names

    verifyPackage :: PackageSet -> PackageConfig -> PackageName -> IO ()
    verifyPackage db pkg name = do
      let
        dirFor pkgName =
          case Map.lookup pkgName db of
            Nothing -> error ("verifyPackageSet: no directory for " <> show pkgName)
            Just pkgInfo -> performInstall (set pkg) pkgName pkgInfo
      echoT ("Verifying package " <> runPackageName name)
      dependencies <- map fst <$> getTransitiveDeps db [name]
      dirs <- mapConcurrently dirFor dependencies
      let srcGlobs = map (pathToTextUnsafe . (</> ("src" </> "**" </> "*.purs"))) dirs
      procs "purs" ("compile" : srcGlobs) empty

data BowerInfoRepo = BowerInfoRepo
  { url :: Text
  } deriving (Show, Eq, Generic, Aeson.FromJSON)

data BowerInfo = BowerInfo
  { bower_name         :: Text
  , bower_repository   :: BowerInfoRepo
  , bower_dependencies :: Map.Map Text Text
  , bower_version      :: Maybe Text
  } deriving (Show, Eq, Generic)
instance Aeson.FromJSON BowerInfo where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions
    { fieldLabelModifier = drop 6
    }

data BowerOutput = BowerOutput
  { latest :: BowerInfo
  } deriving (Show, Eq, Generic, Aeson.FromJSON)

addFromBower :: String -> IO ()
addFromBower arg = do
  echoT $ "Adding package " <> name <> " at " <> (fromMaybe "latest" version) <> " from Bower..."
  let bowerProc = inproc "bower" [ "info", T.pack arg, "--json", "-l=error" ] empty
  result <- fold <$> shellToIOText bowerProc
  if T.null result
    then exitWithErr "Error: Does the package exist on Bower?"
    else do
      let result' = do
            bowerInfo <- case version of
              Just _ -> Aeson.eitherDecodeStrict (encodeUtf8 result) :: Either String BowerInfo
              Nothing -> latest <$> Aeson.eitherDecodeStrict (encodeUtf8 result) :: Either String BowerInfo
            version' <- note "Unable to infer the package version" $ ("v" <>) <$> bower_version bowerInfo <|> version
            pkgName <- mkPackageName' $ bower_name bowerInfo
            packageNames <- traverse mkPackageName' $ Map.keys (bower_dependencies bowerInfo)
            pure $
              ( pkgName
              , PackageInfo
                (T.replace "git:" "https:" . url $ bower_repository bowerInfo)
                version'
                packageNames
              )
      case result' of
        Right (pkgName, info) -> do
          db <- readLocalPackageSet
          writeLocalPackageSet $ Map.insert pkgName info db
          echoT $ "Successfully wrote " <> runPackageName pkgName <> " to package set."
        Left errors -> echoT $ "Errors processing Bower Info: " <> (T.pack errors)
  where
    stripBowerNamePrefix s = fromMaybe s $ T.stripPrefix "purescript-" s
    mkPackageName' = Bifunctor.first show . mkPackageName . stripBowerNamePrefix
    parseVersion' s = case s of
      "" -> Nothing
      s' -> Just $ T.tail s'
    (name, version) = Bifunctor.second parseVersion' $ T.breakOn "#" $ T.pack arg

formatPackageFile :: IO ()
formatPackageFile =
    readLocalPackageSet >>= writeLocalPackageSet

data CmdEnv = CmdEnv
  { cmdEnvSet :: PackageSet
  , cmdEnvCfg :: Maybe PackageConfig
  }

type Cmd = ReaderT CmdEnv IO

tryReadPackageFile :: IO (Maybe PackageConfig)
tryReadPackageFile = do
  exists <- testfile packageFile
  if not exists
    then pure Nothing
    else do
      mpkg <- Aeson.eitherDecodeStrict . encodeUtf8 <$> readTextFile packageFile
      case mpkg of
          Left errors -> do
            echoT $ "Unable to parse psc-package.json: " <> T.pack errors
            pure Nothing
          Right pkg -> return (Just pkg)

getPackageSetCmd :: Cmd PackageSet
getPackageSetCmd = asks cmdEnvSet

getPackageCfg :: Cmd (Maybe PackageConfig)
getPackageCfg = asks cmdEnvCfg

getPackageSetName :: Cmd (Maybe Text)
getPackageSetName = asks (fmap set . cmdEnvCfg)

runCmd :: Cmd a -> IO a
runCmd cmd = do
  pkgC <- tryReadPackageFile
  pkgSet <- case pkgC of
    Nothing -> readLocalPackageSet
    Just cfg -> getPackageSet cfg *> readPackageSet cfg
  runReaderT cmd (CmdEnv { cmdEnvSet = pkgSet, cmdEnvCfg = pkgC })

-- Install All

installAll :: Cmd ()
installAll = do
  pkgC <- getPackageCfg
  lift $ for_ pkgC refreshPackageSet
  packageSet <- getPackageSetCmd
  let packages = Map.toList packageSet
  lift $ forConcurrently_ packages . uncurry $ performInstall $ maybe "local" set pkgC
  where
    refreshPackageSet :: PackageConfig -> IO ()
    refreshPackageSet PackageConfig{ source, set } = do
      let pkgDir = ".psc-package" </> fromText set </> ".set"
      exists <- testdir pkgDir
      when exists (rmtree pkgDir)
      void (cloneShallow source set pkgDir)

-- Lint

type Modulename = Text
type PackageModuleMap = Map.Map Modulename PackageName

lint :: Cmd ()
lint = do
  packageSet <- getPackageSetCmd
  let packages = Map.keys packageSet
  db <- packageModuleMap packageSet
  pkgSet <- lift $ IORef.newIORef packageSet

  for_ packages $ \pn -> do
    calculatedDeps <- Set.delete pn <$> dependenciesForPackage db pn
    let specifiedDeps = Set.fromList $ dependencies (packageSet Map.! pn)
    let superfluousDeps = specifiedDeps Set.\\ calculatedDeps
    let missingDeps = calculatedDeps Set.\\ specifiedDeps
    unless (Set.null superfluousDeps) $ lift $ do
      echoT ("Superfluous deps for package: " <> runPackageName pn)
      print superfluousDeps
    unless (Set.null missingDeps) $ lift $ do
      echoT ("Missing deps for package: " <> runPackageName pn)
      print (map runPackageName (Set.toList missingDeps))
    lift $ IORef.modifyIORef pkgSet (\ps -> Map.update (\c -> Just (c { dependencies = Set.toList calculatedDeps})) pn ps)

  result <- lift $ IORef.readIORef pkgSet
  getPackageCfg >>= \case
    Nothing ->
      lift $ writeLocalPackageSet result
    Just pkgC ->
      lift $ writePackageSet pkgC result

sourcesForPackage :: PackageName -> Cmd [Prelude.FilePath]
sourcesForPackage package = do
  db <- getPackageSetCmd
  pkgC <- getPackageCfg
  let
    Just packageInfo = Map.lookup package db
    path = ".psc-package"
      </> fromText (maybe "local" set pkgC)
      </> fromText (runPackageName package)
      </> fromText (version packageInfo)
      </> "src" </> "**" </> "*.purs"
  lift $ glob (encodeString path)

modulesInPackage :: PackageName -> Cmd [(P.ModuleName, [P.ModuleName])]
modulesInPackage package = do
  paths <- sourcesForPackage package
  modules <- lift $ runExceptT $ traverse PIDE.parseImportsFromFile paths
  case modules of
    Left parseError -> error (show parseError)
    Right ms -> pure $ map (Bifunctor.second (map (\(x, _, _) -> x))) ms

modulenamesInPackage :: PackageName -> Cmd (Set.Set Modulename)
modulenamesInPackage package = do
  ms <- modulesInPackage package
  pure $ Set.fromList $ map (P.runModuleName . fst) ms

importsInPackage :: PackageName -> Cmd (Set.Set Modulename)
importsInPackage package = do
  ms <- modulesInPackage package
  pure $ Set.fromList $ filter (not . T.isPrefixOf "Prim") $ map P.runModuleName $ concatMap snd ms

packageModuleMap :: PackageSet -> Cmd PackageModuleMap
packageModuleMap db = do
  xs <- for (Map.keys db) (\k -> do
    modules <- modulenamesInPackage k
    pure (map (,k) (Set.toList modules)))
  pure (Map.fromList (concat xs))

dependenciesForPackage :: PackageModuleMap -> PackageName -> Cmd (Set.Set PackageName)
dependenciesForPackage db package = do
  imports <- importsInPackage package
  pure (Set.map (\k ->
    fromMaybe
      (error ("When figuring out dependencies for: " <> show package
              <> ", failed to find package for the module: "
              <> show k))
      (Map.lookup k db)) imports)

readDhallPackageSet :: IO ()
readDhallPackageSet = do
  dpkgs <- Dhall.input Dhall.auto "./packages.dhall"
  let pkgSet = convertDPackageSet dpkgs
  writeLocalPackageSet pkgSet

convertDPackageSet :: [Package.Package] -> PackageSet
convertDPackageSet dpkgs =
  Map.fromList
  $ map (\dpkg ->
            ( PackageName (Package.name dpkg)
            , PackageInfo { repo = Package.repo dpkg
                          , version = Package.version dpkg
                          , dependencies = map PackageName (Package.dependencies dpkg)
                          })
        ) dpkgs
