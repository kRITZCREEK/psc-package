{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}

module Package (Package(..), PackageInfo(..), mergeInfo, extractInfo, interpret, _prelude) where

import Dhall as Dhall

import qualified Data.Text as T
import Types (PackageName(..))

data PackageInfo = PackageInfo
  { infoRepo         :: T.Text
  , infoVersion      :: T.Text
  , infoDependencies :: [PackageName]
  } deriving (Show, Eq)

data Package = Package
  { pkgName         :: PackageName
  , pkgRepo         :: T.Text
  , pkgVersion      :: T.Text
  , pkgDependencies :: [PackageName]
  } deriving (Show)

extractInfo :: Package -> (PackageName, PackageInfo)
extractInfo Package{..} = (pkgName, PackageInfo pkgRepo pkgVersion pkgDependencies)

mergeInfo :: PackageName -> PackageInfo -> Package
mergeInfo name PackageInfo{..} = Package name infoRepo infoVersion infoDependencies

interpret :: Dhall.Type Package
interpret = Dhall.record
  ( Package <$> fmap PackageName (Dhall.field "name" Dhall.strictText)
            <*> Dhall.field "repo" Dhall.strictText
            <*> Dhall.field "version" Dhall.strictText
            <*> fmap (map PackageName) (Dhall.field "dependencies" (Dhall.list Dhall.strictText)))

_prelude :: Package
_prelude = Package (PackageName "prelude") "https://github.com/purescript/purescript-prelude.git" "v4.0.0" []

_imp :: IO ()
_imp = do
  cfg <- input (Dhall.list interpret) "./packages.dhall"
  print (cfg :: [Package])
