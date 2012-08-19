{-# LANGUAGE OverloadedStrings #-}

module Shorthand.FontSpec (main) where

import Test.Hspec
import Shorthand
import Data.Maybe

parse = fromJust . parseFont
parseSingle f = fromJust . f . parse

style = parseSingle getFontStyle

main = hspec $ do
  describe "style" $ do
    it "parses italic" $ do
      style "italic" == Italic

    it "parses inherit" $ do
      style "inherit" == InheritStyle