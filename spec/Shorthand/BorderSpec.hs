{-# LANGUAGE OverloadedStrings #-}

module Shorthand.BorderSpec (main) where

import Test.Hspec
import Shorthand
import Data.Maybe

parse = fromJust . parseBorder
parseSingle f = fromJust . f . parse

parseWidth = parseSingle getWidth

parseBorderWidth' = fromJust . parseBorderWidth

main = hspec $ do
  describe "border width" $ do
    it "parses thin" $ do
      parseWidth "thin" == BorderWidth (Just Thin) (Just Thin) (Just Thin) (Just Thin)

  describe "border-width" $ do
    it "parses 1" $ do
      parseBorderWidth' "thin" == BorderWidth (Just Thin) Nothing Nothing Nothing

    it "parses 3" $ do
      parseBorderWidth' "thin thick medium" == BorderWidth (Just Thin) (Just Thick) (Just Medium) Nothing

    it "parses 4" $ do
      parseBorderWidth' "thin thick medium thin" == BorderWidth (Just Thin) (Just Thick) (Just Medium) (Just Thin)

    it "ignores 5th" $ do
      parseBorderWidth' "thin thick medium thin thick" == BorderWidth (Just Thin) (Just Thick) (Just Medium) (Just Thin)

  describe "border" $ do
    it "parses inherit" $ do
      parse "inherit" == InheritBorder
