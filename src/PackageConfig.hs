{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}

module PackageConfig (PackageConfig(..), read, readPackageSet) where

import Prelude hiding (read)

import Dhall as Dhall
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Package (Package)
import qualified Package as Package
import PackageSet (PackageSet)
import qualified PackageSet as PackageSet

import Types (PackageName(..))

data PackageConfig = PackageConfig
  { packageSet   :: [Package]
  , dependencies :: [PackageName]
  } deriving (Show)

-- | Reads a PackageConfig from the given Dhall expression:
-- >>> PackageC.read "./psc-package.dhall"
read :: T.Text -> IO PackageConfig
read path = input interpret (TL.fromStrict path)

readPackageSet :: T.Text -> IO ([PackageSet.Warning], PackageSet)
readPackageSet = fmap (PackageSet.fromPackages . packageSet) . read

interpret :: Dhall.Type PackageConfig
interpret = Dhall.record
  (PackageConfig
    <$> Dhall.field "packageSet" (Dhall.list Package.interpret)
    <*> fmap (map PackageName) (Dhall.field "dependencies" (Dhall.list Dhall.strictText)))

-- for GHCI purposes
_imp :: IO ()
_imp = read "./psc-package.dhall" >>= print
