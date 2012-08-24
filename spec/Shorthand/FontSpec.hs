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
size = parseSingle getFontSize

main = hspec $ do
  describe "style" $ do
    it "parses italic" $ do
      style "italic" == ItalicStyle

    it "parses inherit" $ do
      style "inherit" == InheritStyle

  describe "variant" $ do
    it "parses normal" $ do
      variant "italic normal" == NormalVariant

    it "parses inherit" $ do
      variant "inherit inherit" == InheritVariant

  describe "weight" $ do
    it "parses bold" $ do
      weight "bold" == BoldWeight

    it "parses number" $ do
      weight "400" == NumberWeight 400

    it "parses inherit" $ do
      weight "inherit inherit inherit" == InheritWeight

  describe "size" $ do
    it "parses absolute value" $ do
      size "x-small" == XSmallSize
