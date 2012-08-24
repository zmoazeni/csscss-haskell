{-# LANGUAGE OverloadedStrings #-}

module Shorthand.FontSpec (main) where

import Test.Hspec
import Shorthand
import Data.Maybe

parse = fromJust . parseFont
parseSingle f = fromJust . f . parse

style = parseSingle getFontStyle
variant = parseSingle getFontVariant
weight = parseSingle getFontWeight
size = getFontSize . fromJust . parseFont

main = hspec $ do
  describe "style" $ do
    it "parses italic" $ do
      style "italic small" == ItalicStyle

    it "parses inherit" $ do
      style "inherit small" == InheritStyle

  describe "variant" $ do
    it "parses normal" $ do
      variant "italic normal small" == NormalVariant

    it "parses inherit" $ do
      variant "inherit inherit small" == InheritVariant

  describe "weight" $ do
    it "parses bold" $ do
      weight "bold small" == BoldWeight

    it "parses number" $ do
      weight "400 small" == NumberWeight 400

    it "parses inherit" $ do
      weight "inherit inherit inherit small" == InheritWeight

  describe "size" $ do
    it "parses absolute value" $ do
      size "x-small" == XSmallSize

    it "parses relative value" $ do
      size "larger" == LargerSize

    it "parses length" $ do
      size "10px" == LengthSize (Length 10 PX)

    it "parses percent" $ do
      size "44%" == PercentSize (Percent 44)

    it "parses inherit" $ do
      size "inherit inherit inherit inherit" == InheritSize


