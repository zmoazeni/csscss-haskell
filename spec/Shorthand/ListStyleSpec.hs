{-# LANGUAGE OverloadedStrings #-}

module Shorthand.ListStyleSpec (main) where

import Test.Hspec
import Shorthand
import Data.Maybe

parse = fromJust . parseListStyle
parseSingle f = fromJust . f . parse

lsType     = parseSingle getListStyleType
lsPosition = parseSingle getListStylePosition
lsImage    = parseSingle getListStyleImage

main = hspec $ do
  describe "longhand" $ do
    it "parses longhand" $ do
      parse "disc outside none" == ListStyle (Just DiscLSType) (Just OutsideLSPos) (Just NoneImage)

    it "parses inherit" $ do
      parse "inherit" == InheritListStyle

  describe "type" $ do
    it "parses disc" $ do
      lsType "disc" == DiscLSType

    it "parses inherit" $ do
      lsType "inherit outside" == InheritLSType

  describe "position" $ do
    it "parses outside" $ do
      lsPosition "outside" == OutsideLSPos

    it "parses inherit" $ do
      lsPosition "disc inherit" == InheritLSPos

  describe "image" $ do
    it "parses image" $ do
      lsImage "url(foo.jpg)" == Url "foo.jpg"

    it "parses inherit" $ do
      lsImage "disc outside inherit" == InheritImage

