{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module PscPackage.Client where

import           Control.Concurrent.Async (forConcurrently_)
import           Control.Monad.Reader (ReaderT, runReaderT, asks, lift)
import qualified Data.Graph as Graph
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Package (PackageInfo(..))
import           PackageConfig (PackageConfig)
import qualified PackageConfig as PackageConfig
import           PackageSet (PackageSet)
import qualified PackageSet as PackageSet
import           PscPackage.Git (cloneShallow)
import qualified Turtle
import           Turtle hiding (arg, fold, s, x)
import           Types (PackageName(..), runPackageName)

{-
psc-package install [package-name]
psc-package init
psc-package updates
psc-package available
-}

type Env = PackageConfig
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
  cfg <- readPackageConfig
  runReaderT action cfg

askPackageSet :: Cmd PackageSet
askPackageSet = asks (snd . PackageSet.fromPackages . PackageConfig.packageSet)

askDependencies :: Cmd [PackageName]
askDependencies = asks PackageConfig.dependencies

readPackageConfig :: IO PackageConfig
readPackageConfig = do
  exists <- testfile "psc-package.dhall"
  unless exists $ do
    -- TODO Show the path we tried here
    exitWithErr "Failed to find your package config at psc-package.dhall."
  PackageConfig.read "./psc-package.dhall"

install :: IO ()
install = runCmd installImpl

installAll :: IO ()
installAll = runCmd installAllImpl

installImpl :: Cmd ()
installImpl = do
  deps <- askDependencies
  packageSet <- askPackageSet
  lift $ forConcurrently_ deps $ \pn ->
    case Map.lookup pn packageSet of
      Nothing ->
        exitWithErr $ "Failed to find dependency " <> runPackageName pn <> " in package-set"
      Just info ->
        performInstall pn info

installAllImpl :: Cmd ()
installAllImpl = do
  pkgSet <- askPackageSet
  let packages = Map.toList pkgSet
  lift $ forConcurrently_ packages (uncurry performInstall)

performInstall :: PackageName -> PackageInfo -> IO Turtle.FilePath
performInstall pkgName PackageInfo{ infoRepo, infoVersion } = do
  let pkgDir = ".psc-package" </> fromText (runPackageName pkgName) </> fromText infoVersion
  exists <- testdir pkgDir
  unless exists . void $ do
    echoT ("Installing " <> runPackageName pkgName)
    cloneShallow infoRepo infoVersion pkgDir
  pure pkgDir

echoT :: Text -> IO ()
echoT = Turtle.printf (Turtle.s % "\n")

exitWithErr :: Text -> IO a
exitWithErr errText = errT errText >> exit (ExitFailure 1)
  where errT = traverse Turtle.err . textToLines
