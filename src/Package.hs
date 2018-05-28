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

imp :: IO ()
imp = do
  cfg <- input auto "./packages.dhall"
  print (cfg :: [Package])
