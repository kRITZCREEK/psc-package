{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Package (Package(..)) where

import Dhall

import Data.Text as T

data Package = Package
  { name         :: T.Text
  , repo         :: T.Text
  , version      :: T.Text
  , dependencies :: [T.Text]
  } deriving (Show, Generic, Interpret)

_prelude :: Package
_prelude = Package "prelude" "https://github.com/purescript/purescript-prelude.git" "v4.0.0" []

_imp :: IO ()
_imp = do
  cfg <- input auto "./packages.dhall"
  print (cfg :: [Package])
