{-# LANGUAGE OverloadedStrings #-}

module Development.CSSCSS.RulesetsSpec where

import Test.Hspec
import Development.CSSCSS.Rulesets

main = hspec spec

spec =
  describe "building rulesets" $ do
    it "builds cleanly" $ do
      buildRulesets [("body", [("display", "block")])] == [Ruleset "body" [Rule "display" "block"]]

    it "sorts the rules alphabetically" $ do
      buildRulesets [("body", [("display", "block"), ("background-color", "black")])] == [
        Ruleset "body" [
           Rule "background-color" "black",
           Rule "display" "block"
           ]
        ]

    it "sorts rulesets alphabetically" $ do
      buildRulesets [("p", [("visibility", "hidden"), ("background", "1px solid black")]),
                     ("body", [("display", "block"), ("background-color", "black")])] == [
        Ruleset "body" [
           Rule "background-color" "black",
           Rule "display" "block"
           ],
        Ruleset "p" [
           Rule "background" "1px solid black",
           Rule "visibility" "hidden"
           ]
        ]

