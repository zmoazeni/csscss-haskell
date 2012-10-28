{-# LANGUAGE OverloadedStrings #-}

module Shorthand.BackgroundSpec (main, spec) where

import Test.Hspec
import Shorthand
import Data.Maybe

-- trace (show $ parseBackground "black") False

parse = fromJust . parseBackground
parseSingle f = fromJust . f . parse

parseColor      = parseSingle getColor
parseImage      = parseSingle getImage
parseRepeat     = parseSingle getRepeat
parseAttachment = parseSingle getAttachment
parsePosition   = parseSingle getPosition

main = hspec spec

spec = describe "background spec" $ do
  describe "background" $ do
    it "parses inherit background" $ do
      parse "inherit" == InheritBackground

    it "parses longhand" $ do
      parse "#fa1023 url(foo.jpeg) repeat-x scroll left top" ==
        Background (Just (Hex "fa1023")) (Just (Url "foo.jpeg")) (Just RepeatX) (Just Scroll) (Just (Position (LeftPoint, (Just TopPoint))))

    it "parses longhand2" $ do
      parse "url(foo.jpeg) repeat-x 10px" ==
        Background Nothing (Just (Url "foo.jpeg")) (Just RepeatX) Nothing (Just (Position (HLength (Length 10 PX), Nothing)))

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
      parseColor "inherit left" == InheritColor

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

  describe "background attachment" $ do
    it "parses scroll" $ do
      parseAttachment "scroll" == Scroll

    it "parses fixed" $ do
      parseAttachment "fixed" == Fixed

    it "parses inherit" $ do
      parseAttachment "black none repeat inherit" == InheritAttachment

  describe "background position" $ do
    it "parses single" $ do
      parsePosition "left" == Position (LeftPoint, Nothing)

    it "parses two" $ do
      parsePosition "left top" == Position (LeftPoint, Just TopPoint)

    it "parses single %" $ do
      parsePosition "10%" == Position (HPercent (Percent 10), Nothing)

    it "parses two %" $ do
      parsePosition "10% 50%" == Position (HPercent (Percent 10), Just (VPercent (Percent 50)))

    it "parses single length" $ do
      parsePosition "10px" == Position (HLength (Length 10 PX), Nothing)

    it "parses two lengths" $ do
      parsePosition "10px 5em" == Position (HLength (Length 10 PX), Just (VLength (Length 5 EM)))

    it "parses two different" $ do
      parsePosition "left 10%" == Position (LeftPoint, Just (VPercent (Percent 10)))

    it "parses inherit" $ do
      parsePosition "black none repeat fixed inherit" == InheritPosition
