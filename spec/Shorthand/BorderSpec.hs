{-# LANGUAGE OverloadedStrings #-}

module Shorthand.BorderSpec (main) where

import Test.Hspec
import Shorthand
import Data.Maybe

parse = fromJust . parseBorder
parseSingle f = fromJust . f . parse

width = parseSingle getWidth

borderWidth = fromJust . parseBorderWidth
borderStyle = fromJust . parseBorderStyle

main = hspec $ do
  describe "border-width" $ do
    it "parses 1" $ do
      borderWidth "thin" == BorderWidth (Just Thin) Nothing Nothing Nothing

    it "parses 4" $ do
      borderWidth "thin thick medium thin" == BorderWidth (Just Thin) (Just Thick) (Just Medium) (Just Thin)

    it "ignores 5th" $ do
      borderWidth "thin thick medium thin thick" == BorderWidth (Just Thin) (Just Thick) (Just Medium) (Just Thin)

    it "ignores unknown" $ do
      borderWidth "thin thick foo thin" == BorderWidth (Just Thin) (Just Thick) Nothing Nothing

  describe "border-style" $ do
    it "parses 1" $ do
      borderStyle "dashed" == BorderStyle (Just Dashed) Nothing Nothing Nothing

    it "parses 4" $ do
      borderStyle "dashed double solid groove" == BorderStyle (Just Dashed) (Just Double) (Just Solid) (Just Groove)

    it "ignores 5th" $ do
      borderStyle "dashed double solid groove solid" == BorderStyle (Just Dashed) (Just Double) (Just Solid) (Just Groove)

    it "ignores unknown" $ do
      borderStyle "dashed double foo groove" == BorderStyle (Just Dashed) (Just Double) Nothing Nothing

  describe "border" $ do
    it "parses inherit" $ do
      parse "inherit" == InheritBorder

    it "parses all" $ do
      parse "thin dashed" == Border
        (Just (BorderWidth (Just Thin) (Just Thin) (Just Thin) (Just Thin)))
        (Just (BorderStyle (Just Dashed) (Just Dashed) (Just Dashed) (Just Dashed)))
