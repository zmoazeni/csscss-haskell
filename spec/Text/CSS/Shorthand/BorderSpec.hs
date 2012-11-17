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
      borderWidth "thin" == BorderWidths Thin Thin Thin Thin

    it "parses 2" $ do
      borderWidth "thin thick" == BorderWidths Thin Thick Thin Thick

    it "parses 3" $ do
      borderWidth "thin thick medium" == BorderWidths Thin Thick Medium Thick

    it "parses 4" $ do
      borderWidth "thin thick medium thin" == BorderWidths Thin Thick Medium Thin

    it "ignores 5th" $ do
      borderWidth "thin thick medium thin thick" == BorderWidths Thin Thick Medium Thin

    it "ignores unknown" $ do
      borderWidth "thin thick foo thin" == BorderWidths Thin Thick Thin Thick

    it "parses lengths" $ do
      borderWidth "thin 10px medium 20em" == BorderWidths Thin (WLength $ Length 10 PX) Medium (WLength $ Length 20 EM)

    it "parses lengths" $ do
      borderWidth "thin 10px medium 20em" == BorderWidths Thin (WLength $ Length 10 PX) Medium (WLength $ Length 20 EM)

    it "parses inherit" $ do
      borderWidth "inherit" == InheritBorderWidth

  describe "border-style" $ do
    it "parses 1" $ do
      borderStyle "dashed" == BorderStyles Dashed Dashed Dashed Dashed

    it "parses 2" $ do
      borderStyle "dashed solid" == BorderStyles Dashed Solid Dashed Solid

    it "parses 3" $ do
      borderStyle "dashed solid groove" == BorderStyles Dashed Solid Groove Solid

    it "parses 4" $ do
      borderStyle "dashed double solid groove" == BorderStyles Dashed Double Solid Groove

    it "ignores 5th" $ do
      borderStyle "dashed double solid groove solid" == BorderStyles Dashed Double Solid Groove

    it "ignores unknown" $ do
      borderStyle "dashed double foo groove" == BorderStyles Dashed Double Dashed Double

    it "parses inherit" $ do
      borderStyle "inherit" == InheritBorderStyle

  describe "border" $ do
    it "parses inherit" $ do
      parse "inherit" == InheritBorder

    it "parses inherit width and style" $ do
      parse "inherit inherit" == Border (Just InheritBorderWidth) (Just InheritBorderStyle)

    it "parses width and inherit style" $ do
      parse "thick inherit" == Border
        (Just (BorderWidths Thick Thick Thick Thick))
        (Just InheritBorderStyle)

    it "parses width and inherit style" $ do
      parse "inherit dashed" == Border
        (Just InheritBorderWidth)
        (Just (BorderStyles Dashed Dashed Dashed Dashed))

    it "parses all" $ do
      parse "thin dashed" == Border
        (Just (BorderWidths Thin Thin Thin Thin))
        (Just (BorderStyles Dashed Dashed Dashed Dashed))

    it "returns Nothing if it can't parse" $ do
      parseBorder "foobar" == Nothing

  describe "getLonghandRules" $ do
    it "all values" $ do
      let border = Border (Just (BorderWidths Thin Medium Thick (WLength $ Length 10 PX)))
                          (Just (BorderStyles Dashed None Solid Double))
      getLonghandRules border == [
        Rule "border-top-width"    "thin",
        Rule "border-right-width"  "medium",
        Rule "border-bottom-width" "thick",
        Rule "border-left-width"   "10px",
        Rule "border-top-style"    "dashed",
        Rule "border-right-style"  "none",
        Rule "border-bottom-style" "solid",
        Rule "border-left-style"   "double"]

    it "missing styles" $ do
      let border = Border (Just (BorderWidths Thin Thick Thin (WLength $ Length 10 PX))) Nothing
      getLonghandRules border == [
        Rule "border-top-width" "thin",
        Rule "border-right-width" "thick",
        Rule "border-bottom-width" "thin",
        Rule "border-left-width" "10px"]
