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

    it "parses rgb function" $ do
      fromJust (parseBackground "rgb (  255, 0, 10)") == RGB "255" "0" "10"

    it "parses rgb% function" $ do
      fromJust (parseBackground "rgb   (80%, 10%, 5%  )") == RGBP "80" "10" "5"

    it "parses known keywords" $ do
      fromJust (parseBackground "black") == Hex "000000"

    it "parses inherit" $ do
      fromJust (parseBackground "inherit") == InheritColor
