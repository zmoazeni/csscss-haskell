{-# LANGUAGE OverloadedStrings #-}

module ValueParserSpec (main) where

import Test.Hspec
import ValueParser
import System.IO.Unsafe
import Debug.Trace
import Data.Maybe

-- trace (show $ parseBackground "black") False

parseSingle f = fromJust . f . fromJust . parseBackground
parseColor  = parseSingle getColor
parseImage  = parseSingle getImage
parseRepeat = parseSingle getRepeat

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

    it "parses inherit" $ do
      parseImage "black inherit" == InheritImage

    it "parses none" $ do
      parseImage "none" == NoneImage

  describe "background repeat" $ do
    it "parses repeat" $ do
      parseRepeat "repeat" == Repeat

    it "parses repeat-x" $ do
      parseRepeat "repeat-x" == RepeatX

    it "parses repeat-y" $ do
      parseRepeat "repeat-y" == RepeatY

    it "parses no-repeat" $ do
      parseRepeat "no-repeat" == NoRepeat

    it "parses inherit" $ do
      parseRepeat "black none inherit" == InheritRepeat

