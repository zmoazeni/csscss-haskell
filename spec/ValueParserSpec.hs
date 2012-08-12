{-# LANGUAGE OverloadedStrings #-}

module ValueParserSpec (main) where

import Test.Hspec
import ValueParser
import System.IO.Unsafe
import Debug.Trace
import Data.Maybe

-- trace (show $ parseBackground "black") False

parseColor = fromJust . getColor . fromJust . parseBackground
parseImage = fromJust . getImage . fromJust . parseBackground

main = hspec $ do
  describe "background color" $ do
    it "parses 6 char hex" $ do
      parseColor "#f12fff" == Hex "f12fff"

    it "parses 3 char hex" $ do
      parseColor "#f12" == Hex "ff1122"

    it "parses rgb function" $ do
      parseColor "rgb (  255, 0, 10)" == RGB "255" "0" "10"

    it "parses rgb% function" $ do
      parseColor "rgb   (  80%, 10%, 5%  )" == RGBP "80" "10" "5"

    it "parses known keywords" $ do
      parseColor "black" == Hex "000000"

    it "parses inherit" $ do
      parseColor "inherit" == InheritColor

  describe "background image" $ do
    it "parses url single quote" $ do
      parseImage "url (   'http://foo.com'  )" == Url "http://foo.com"

    it "parses url double quote" $ do
      parseImage "url (   \"http://foo.com\"  )" == Url "http://foo.com"

    it "parses url no quote" $ do
      parseImage "url (   http://foo.com  )" == Url "http://foo.com"

