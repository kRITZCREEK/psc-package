{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module D where

import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Text.Lazy.Builder as Builder
import qualified Dhall.Core as Dhall
import Dhall.Core
import Dhall.Pretty (prettyExpr)
import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Package (Package(..))
import qualified Package as Package
import PackageConfig (PackageConfig(..))
import PackageSet (PackageSet)
import Types

import qualified Data.HashMap.Strict.InsOrd as IOHM

printPackageConfig :: PackageConfig -> Text
printPackageConfig = printExpr . mkConfig

printPackageSet :: PackageSet -> Text
printPackageSet = printExpr . mkPackageSet

mkConfig :: PackageConfig -> Expr s Dhall.Import
mkConfig pkgCfg =
  RecordLit
    (IOHM.fromList
      [ ("packageSet", mkPackageSet' (PackageConfig.packageSet pkgCfg))
      , ("dependencies", listLit [])
      , ("metadata", listLit [])
      ])

mkPackageSet :: PackageSet -> Expr s Dhall.Import
mkPackageSet pkgSet =
  mkPackageSet' (map (uncurry Package.mergeInfo) (Map.toAscList pkgSet))

mkPackageSet' :: [Package] -> Expr s Dhall.Import
mkPackageSet' = ListLit Nothing . Seq.fromList . map mkPackage

mkPackage :: Package -> Expr s Dhall.Import
mkPackage Package{..} =
  RecordLit
    (IOHM.fromList
      [ ("name", textLit (runPackageName pkgName))
      , ("repo", textLit pkgRepo)
      , ("version", textLit pkgVersion)
      , ("dependencies", listLit (map (textLit . runPackageName) pkgDependencies))
      ])

listLit :: [Expr s a] -> Expr s a
listLit exprs = ListLit (Just Dhall.Text) (Seq.fromList exprs)

textLit :: Text -> Expr s a
textLit s = TextLit (Chunks [] (Builder.fromText s))

opts :: Pretty.LayoutOptions
opts =
  Pretty.defaultLayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }

printExpr :: Expr s Dhall.Import -> Text
printExpr = renderStrict . Pretty.layoutSmart opts . prettyExpr
