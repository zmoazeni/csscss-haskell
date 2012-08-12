{-# LANGUAGE OverloadedStrings #-}

module ValueParserSpec where

import Test.Hspec
import ValueParser
import System.IO.Unsafe
import Debug.Trace
import Data.Maybe

main = hspec $ do
  describe "background" $ do
    it "playing with parser" $ do
      -- trace (show $ parseBackground "black") False
      fromJust (parseBackground "black") == Hex "000000"
      
    it "playing with parser2" $ do
      fromJust (parseBackground "inherit") == Inherit
