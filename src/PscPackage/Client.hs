{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module PscPackage.Client where

import           Turtle hiding (arg, fold, s, x)
import           Control.Concurrent.Async (forConcurrently_)
import Data.Map (Map)
import Types (PackageName(..), runPackageName)
import qualified Data.Map as Map
import qualified Turtle
import Package (PackageInfo(..))
import PackageConfig (PackageConfig)
import qualified PackageConfig as PackageConfig
import PackageSet (PackageSet)
import qualified PackageSet as PackageSet
import Control.Monad.Reader (ReaderT, runReaderT, asks, lift)
import PscPackage.Git (cloneShallow)

{-
psc-package install [package-name]
psc-package init
psc-package updates
psc-package available
-}

type Env = PackageConfig
type Cmd = ReaderT Env IO

runCmd :: Cmd a -> IO a
runCmd action = do
  cfg <- readPackageConfig
  runReaderT action cfg

askPackageSet :: Cmd PackageSet
askPackageSet = asks (snd . PackageSet.fromPackages . PackageConfig.packageSet)

readPackageConfig :: IO PackageConfig
readPackageConfig = do
  exists <- testfile "psc-package.dhall"
  unless exists $ do
    -- TODO Show the path we tried here
    exitWithErr "Failed to find your package config at psc-package.dhall."
  PackageConfig.read "./psc-package.dhall"

exitWithErr :: Text -> IO a
exitWithErr errText = errT errText >> exit (ExitFailure 1)
  where errT = traverse Turtle.err . textToLines

install :: Maybe PackageName -> IO ()
install = runCmd . installImpl

installImpl :: Maybe PackageName -> Cmd ()
installImpl pn = case pn of
  Nothing -> installAll
  Just pn -> undefined

installAll :: Cmd ()
installAll = do
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
