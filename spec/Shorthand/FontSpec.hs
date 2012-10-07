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
lineHeight = parseSingle getLineHeight
family = parseSingle getFontFamily
size = parseSingle getFontSize
systemFont = parseSingle getSystemFont


main = hspec $ do
  describe "longhand" $ do
    it "parses longhand" $ do
      parse "italic bold 36px / 15px 'foo', serif" == Font (Just ItalicStyle) Nothing (Just BoldWeight) (Just $ LengthSize (Length 36 PX)) (Just $ LengthLH (Length 15 PX)) (Just [FontName "foo", SerifName]) Nothing

    it "parses inherit" $ do
      parse "inherit" == InheritFont

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

  describe "line height" $ do
    it "parses with space" $ do
      lineHeight "10px / 12px" == LengthLH (Length 12 PX)

    it "parses without space" $ do
      lineHeight "10px/15px" == LengthLH (Length 15 PX)

    it "parses normal" $ do
      lineHeight "10px/normal" == NormalLH

    it "parses percent" $ do
      lineHeight "10px/44%" == PercentLH (Percent 44)

    it "parses number" $ do
      lineHeight "10px/44" == NumberLH 44

    it "parses inherit" $ do
      lineHeight "10px/inherit" == InheritLH

  describe "font family" $ do
    it "parses double quoted name" $ do
      family "10px \"foo\"" == [FontName "foo"]

    it "parses single quoted name" $ do
      family "10px 'foo'" == [FontName "foo"]

    it "parses unquoted name" $ do
      family "10px foo" == [FontName "foo"]

    it "parses list" $ do
      family "10px foo , \"foo2\",'New Century Schoolbook'" == [FontName "foo", FontName "foo2", FontName "New Century Schoolbook"]

    it "parses generic" $ do
      family "10px serif" == [SerifName]

    it "parses family names and generics list" $ do
      family "10px serif, \"Times New Roman\", cursive, Lucida, fantasy, \"fantasy\"" == [SerifName, FontName "Times New Roman", CursiveName, FontName "Lucida", FantasyName, FontName "fantasy"]

    it "parses inherit" $ do
      family "10px inherit" == [InheritFamily]

  describe "system font" $ do
    it "parses caption" $ do
      systemFont "caption" == CaptionFont

    it "parses icon" $ do
      systemFont "icon" == IconFont