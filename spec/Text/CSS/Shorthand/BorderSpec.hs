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
borderColor = fromJust . parseBorderColors

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

  describe "border-color" $ do
    it "parses 1" $ do
      borderColor "black" == BorderColors (Hex "000000") (Hex "000000") (Hex "000000") (Hex "000000")

    it "parses 2" $ do
      borderColor "black white" == BorderColors (Hex "000000") (Hex "ffffff") (Hex "000000") (Hex "ffffff")

    it "parses 3" $ do
      borderColor "black white red" == BorderColors (Hex "000000") (Hex "ffffff") (Hex "ff0000") (Hex "ffffff")

    it "parses 4" $ do
      borderColor "black white red #ff1212" == BorderColors (Hex "000000") (Hex "ffffff") (Hex "ff0000") (Hex "ff1212")

    it "ignores 5th" $ do
      borderColor "black white red #ff1212 #abb" == BorderColors (Hex "000000") (Hex "ffffff") (Hex "ff0000") (Hex "ff1212")

    it "ignores unknown" $ do
      borderColor "black white lightred red" == BorderColors (Hex "000000") (Hex "ffffff") (Hex "000000") (Hex "ffffff")

    it "parses inherit" $ do
      borderColor "inherit" == InheritBorderColor

  describe "border" $ do
    it "parses inherit" $ do
      parse "inherit" == InheritBorder

    it "parses inherit width and style and color" $ do
      parse "inherit inherit inherit" == Border (Just InheritBorderWidth) (Just InheritBorderStyle) (Just InheritBorderColor)

    it "parses width and inherit style" $ do
      parse "thick inherit" == Border
        (Just (BorderWidths Thick Thick Thick Thick))
        (Just InheritBorderStyle)
        Nothing

    it "parses width and inherit style" $ do
      parse "inherit dashed" == Border
        (Just InheritBorderWidth)
        (Just (BorderStyles Dashed Dashed Dashed Dashed))
        Nothing

    it "parses all" $ do
      parse "thin dashed black" == Border
        (Just (BorderWidths Thin Thin Thin Thin))
        (Just (BorderStyles Dashed Dashed Dashed Dashed))
        (Just (BorderColors (Hex "000000") (Hex "000000") (Hex "000000") (Hex "000000")))

    it "returns Nothing if it can't parse" $ do
      parseBorder "foobar" == Nothing

  describe "getLonghandRules" $ do
    it "all values" $ do
      let border = Border (Just (BorderWidths Thin Medium Thick (WLength $ Length 10 PX)))
                          (Just (BorderStyles Dashed None Solid Double))
                          (Just (BorderColors (Hex "ffffff") (RGB "255" "255" "255") (Hex "000000") (RGBP "100" "100" "100")))
      getLonghandRules border == [
        Rule "border-top-width"    "thin",
        Rule "border-right-width"  "medium",
        Rule "border-bottom-width" "thick",
        Rule "border-left-width"   "10px",
        Rule "border-top-style"    "dashed",
        Rule "border-right-style"  "none",
        Rule "border-bottom-style" "solid",
        Rule "border-left-style"   "double",
        Rule "border-top-color"    "#ffffff",
        Rule "border-right-color"  "rgb(255, 255, 255)",
        Rule "border-bottom-color" "#000000",
        Rule "border-left-color"   "rgb(100%, 100%, 100%)"]

    it "missing styles and color" $ do
      let border = Border (Just (BorderWidths Thin Thick Thin (WLength $ Length 10 PX))) Nothing Nothing
      getLonghandRules border == [
        Rule "border-top-width" "thin",
        Rule "border-right-width" "thick",
        Rule "border-bottom-width" "thin",
        Rule "border-left-width" "10px"]
