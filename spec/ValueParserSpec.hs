{-# LANGUAGE OverloadedStrings #-}

module ValueParserSpec where

import Test.Hspec
import ValueParser
import System.IO.Unsafe
import Debug.Trace
import Data.Maybe

-- trace (show $ parseBackground "black") False

main = hspec $ do
  describe "background color" $ do
    it "parses 6 char hex" $ do
      fromJust (parseBackground "#f12fff") == Hex "f12fff"

    it "parses 3 char hex" $ do
      fromJust (parseBackground "#f12") == Hex "ff1122"

    it "parses known keywords" $ do
      fromJust (parseBackground "black") == Hex "000000"
      
    it "parses inherit" $ do
      fromJust (parseBackground "inherit") == Inherit
