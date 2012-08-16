{-# LANGUAGE OverloadedStrings #-}

module Shorthand.BorderSpec (main) where

import Test.Hspec
import Shorthand
import Data.Maybe

parse = fromJust . parseBorder
parseSingle f = fromJust . f . parse

parseWidth = parseSingle getWidth

main = hspec $ do
  describe "border width" $ do
    it "parses thin" $ do
      parseWidth "thin" == BorderWidth (Just Thin) (Just Thin) (Just Thin) (Just Thin)

  describe "border" $ do
    it "parses inherit" $ do
      parse "inherit" == InheritBorder
