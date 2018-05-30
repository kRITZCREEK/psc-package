module PackageSet (PackageSet(..), Warning(..), fromPackages) where

import Package (Package)
import Package as Package
import Data.Map (Map)
import Data.List (foldl')
import qualified Data.Map as Map
import Types (PackageName(..))

data PackageSet = PackageSet (Map PackageName Package)
  deriving (Show)

data Warning = Override PackageName
  deriving (Show)

fromPackages :: [Package] -> ([Warning], PackageSet)
fromPackages = flip foldl' ([], PackageSet Map.empty) (\(warnings, PackageSet packages) package ->
    let
      pkgName = PackageName (Package.name package)
      newWarnings = if Map.member pkgName packages
        then Override pkgName : warnings
        else warnings
      newPackages = Map.insert pkgName package packages
    in
      (newWarnings, PackageSet newPackages))
