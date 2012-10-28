{-# LANGUAGE OverloadedStrings #-}

module Shorthand.BorderSpec (main, spec) where

import Test.Hspec
import Shorthand
import Data.Maybe

parse = fromJust . parseBorder
parseSingle f = fromJust . f . parse

borderWidth = fromJust . parseBorderWidths
borderStyle = fromJust . parseBorderStyles

main = hspec spec

spec = describe "border spec" $ do
  describe "border-width" $ do
    it "parses 1" $ do
      borderWidth "thin" == BorderWidths (Just Thin) Nothing Nothing Nothing

    it "parses 4" $ do
      borderWidth "thin thick medium thin" == BorderWidths (Just Thin) (Just Thick) (Just Medium) (Just Thin)

    it "ignores 5th" $ do
      borderWidth "thin thick medium thin thick" == BorderWidths (Just Thin) (Just Thick) (Just Medium) (Just Thin)

    it "ignores unknown" $ do
      borderWidth "thin thick foo thin" == BorderWidths (Just Thin) (Just Thick) Nothing Nothing

    it "parses lengths" $ do
      borderWidth "thin 10px medium 20em" == BorderWidths (Just Thin) (Just (WLength $ Length 10 PX)) (Just Medium) (Just (WLength $ Length 20 EM))

  describe "border-style" $ do
    it "parses 1" $ do
      borderStyle "dashed" == BorderStyles (Just Dashed) Nothing Nothing Nothing

    it "parses 4" $ do
      borderStyle "dashed double solid groove" == BorderStyles (Just Dashed) (Just Double) (Just Solid) (Just Groove)

    it "ignores 5th" $ do
      borderStyle "dashed double solid groove solid" == BorderStyles (Just Dashed) (Just Double) (Just Solid) (Just Groove)

    it "ignores unknown" $ do
      borderStyle "dashed double foo groove" == BorderStyles (Just Dashed) (Just Double) Nothing Nothing

  describe "border" $ do
    it "parses inherit" $ do
      parse "inherit" == InheritBorder

    it "parses all" $ do
      parse "thin dashed" == Border
        (Just (BorderWidths (Just Thin) (Just Thin) (Just Thin) (Just Thin)))
        (Just (BorderStyles (Just Dashed) (Just Dashed) (Just Dashed) (Just Dashed)))
