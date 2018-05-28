{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}

module PackageConfig (PackageConfig(..), read) where

import Prelude hiding (read)

import Dhall (input, auto, Interpret, Generic)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Package (Package)
import PackageSet (PackageSet)
import qualified PackageSet as PackageSet

data PackageConfig = PackageConfig
  { packageSet   :: [Package]
  , dependencies :: [Text]
  } deriving (Show, Generic, Interpret)

-- | Reads a PackageConfig from the given Dhall expression:
-- >>> PackageC.read "./psc-package.json"
read :: Text -> IO [Package]
read path = input auto (TL.fromStrict path)

readPackageSet :: Text -> IO ([PackageSet.Warning], PackageSet)
readPackageSet = fmap PackageSet.fromPackages . read

-- for GHCI purposes
_imp :: IO ()
_imp = read "./psc-package.dhall" >>= print
