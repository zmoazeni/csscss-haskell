{-# LANGUAGE OverloadedStrings #-}

module Text.CSS.Shorthand.BorderSpec (main, spec) where

import Test.Hspec
import Text.CSS.Shorthand
import Data.Maybe
import Development.CSSCSS.Rulesets

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

  describe "getLonghandRules" $ do
    it "all values" $ do
      let border = Border (Just (BorderWidths (Just Thin) (Just Medium) (Just Thick) (Just . WLength $ Length 10 PX)))
                    (Just (BorderStyles (Just Dashed) (Just None) (Just Solid) (Just Double)))
      getLonghandRules border == [
        Rule "border-top-width"    "thin",
        Rule "border-right-width"  "medium",
        Rule "border-bottom-width" "thick",
        Rule "border-left-width"   "10px",
        Rule "border-top-style"    "dashed",
        Rule "border-right-style"  "none",
        Rule "border-bottom-style" "solid",
        Rule "border-left-style"   "double"]

    it "missing some" $ do
      let border = Border (Just (BorderWidths (Just Thin) Nothing Nothing (Just . WLength $ Length 10 PX)))
                    (Just (BorderStyles (Just Dashed) (Just None) Nothing Nothing))
      getLonghandRules border == [
        Rule "border-top-width"    "thin",
        Rule "border-left-width"   "10px",
        Rule "border-top-style"    "dashed",
        Rule "border-right-style"  "none"]

    it "missing styles" $ do
      let border = Border (Just (BorderWidths (Just Thin) Nothing Nothing (Just . WLength $ Length 10 PX))) Nothing
      getLonghandRules border == [ Rule "border-top-width" "thin", Rule "border-left-width" "10px"]
