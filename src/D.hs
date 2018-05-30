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
import Data.Text.Lazy.Builder as Builder
import qualified Dhall.Core as Dhall
import Dhall.Core
import Dhall.Pretty (prettyExpr)
import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Package (Package(..))

import qualified Data.HashMap.Strict.InsOrd as IOHM

mkPackage :: Package -> Expr s Path
mkPackage Package{..} =
  RecordLit
    (IOHM.fromList
      [ ("name", textLit name)
      , ("repo", textLit repo)
      , ("version", textLit version)
      , ("dependencies", listLit (map textLit dependencies))
      ])

listLit :: [Expr s a] -> Expr s a
listLit exprs = ListLit (Just Dhall.Text) (Seq.fromList exprs)

textLit :: Text -> Expr s a
textLit s = TextLit (Chunks [] (Builder.fromText s))

opts :: Pretty.LayoutOptions
opts =
    Pretty.defaultLayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }

printExpr :: Expr s Path -> Text
printExpr = renderStrict . Pretty.layoutSmart opts . prettyExpr
