{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module PscPackage.Set where

import           Control.Concurrent.Async (forConcurrently_)
import           Control.Monad.Reader (ReaderT, runReaderT, asks, lift)
import           Data.Foldable (for_)
import           Data.Traversable (for)
import qualified Data.Graph as Graph
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import           Package (Package(..), PackageInfo(..))
import qualified Package as Package
import           PackageConfig (PackageConfig)
import qualified PackageConfig as PackageConfig
import           PackageSet (PackageSet)
import qualified PackageSet as PackageSet
import           PscPackage.Git (cloneShallow)
import qualified Turtle
import           Turtle hiding (arg, fold, s, x)
import           Types (PackageName(..), runPackageName)

import qualified Data.Set as Set

ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs
{-
pup set install
pup set updates
pup set lint
-}

type Env = PackageSet
type Cmd = ReaderT Env IO

mkPackageGraph
  :: PackageSet
  -> ( Graph.Graph
     , Graph.Vertex -> (PackageInfo, PackageName, [PackageName])
     , PackageName -> Maybe Graph.Vertex
     )
mkPackageGraph ps =
  Graph.graphFromEdges
  $ map (\(pn, info) ->
           (info, pn, Package.infoDependencies info))
  $ Map.toList ps

transitiveDeps :: PackageSet -> PackageName -> [PackageName]
transitiveDeps ps pn =
  let (graph, vertexToInfo, pnToVertex) = mkPackageGraph ps
  in map ((\(_, pn, _) -> pn) . vertexToInfo) $ Graph.reachable graph (fromMaybe undefined (pnToVertex pn))

runCmd :: Cmd a -> IO a
runCmd action = do
  cfg <- readPackageSet
  runReaderT action cfg

askPackageSet :: Cmd PackageSet
askPackageSet = asks (snd . PackageSet.fromPackages . PackageConfig.packageSet)

askDependencies :: Cmd [PackageName]
askDependencies = asks PackageConfig.dependencies

askTransDependencies :: Cmd [Package]
askTransDependencies = do
  packageSet <- askPackageSet
  deps <- asks PackageConfig.dependencies
  let transDeps = ordNub (foldMap (transitiveDeps packageSet) deps)
  fmap catMaybes $ for transDeps $ \pn ->  do
    case Map.lookup pn packageSet of
      Nothing -> do
        lift $ echoT $ "[WARNING] Failed to find transitive dependency \"" <> runPackageName pn <> "\" in package set. This most likely means there is a problem with the packageset"
        pure Nothing
      Just info ->
        pure (Just (Package.mergeInfo pn info))

readPackageSet :: IO PackageSet
readPackageSet = do
  exists <- testfile "package-set.dhall"
  unless exists $ do
    -- TODO Show the path we tried here
    exitWithErr "Failed to find your package set at package-set.dhall."
  Package.read "./psc-package.dhall"

install :: IO ()
install = runCmd installImpl

installAll :: IO ()
installAll = runCmd installAllImpl

sources :: IO ()
sources = runCmd sourcesImpl

installImpl :: Cmd ()
installImpl = do
  deps <- askTransDependencies
  lift $ forConcurrently_ deps performInstall

installAllImpl :: Cmd ()
installAllImpl = do
  pkgSet <- askPackageSet
  let packages = map (uncurry Package.mergeInfo) (Map.toList pkgSet)
  lift $ forConcurrently_ packages performInstall

performInstall :: Package -> IO Turtle.FilePath
performInstall Package { pkgName, pkgRepo, pkgVersion } = do
  let pkgDir = ".psc-package" </> fromText (runPackageName pkgName) </> fromText pkgVersion
  exists <- testdir pkgDir
  unless exists . void $ do
    echoT ("Installing " <> runPackageName pkgName)
    cloneShallow pkgRepo pkgVersion pkgDir
  pure pkgDir

sourcesImpl :: Cmd ()
sourcesImpl = do
  deps <- askTransDependencies
  for_ deps $ \Package { pkgName, pkgVersion} -> do
    lift $ echoT $ ".psc-package/" <> runPackageName pkgName <> "/" <> pkgVersion <> "/src/**/*.purs"

echoT :: Text -> IO ()
echoT = Turtle.printf (Turtle.s % "\n")

exitWithErr :: Text -> IO a
exitWithErr errText = errT errText >> exit (ExitFailure 1)
  where errT = traverse Turtle.err . textToLines
