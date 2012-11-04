{-# LANGUAGE OverloadedStrings #-}

module RedundancyCalcSpec where

import Test.Hspec
import Rulesets
import RedundancyCalc

rain = hspec spec

spec = describe "redundancy calc" $ do
  describe "matches" $ do
    it "finds shared rules" $ do
      matches [Ruleset ".foo" [Rule "display" "none", Rule "position" "relative"],
               Ruleset ".bar" [Rule "position" "relative", Rule "display" "none"],
               Ruleset ".baz" [Rule "position" "relative"]]
              ==
              [(Ruleset ".foo" [Rule "display" "none", Rule "position" "relative"], [
                  Match 1 ".bar" [Rule "display" "none", Rule "position" "relative"],
                  Match 2 ".baz" [Rule "position" "relative"]]),

               (Ruleset ".bar" [Rule "display" "none", Rule "position" "relative"], [
                  Match 0 ".foo" [Rule "display" "none", Rule "position" "relative"],
                  Match 2 ".baz" [Rule "position" "relative"]]),

               (Ruleset ".baz" [Rule "position" "relative"], [
                  Match 0 ".foo" [Rule "position" "relative"],
                  Match 1 ".bar" [Rule "position" "relative"]])
              ]


    it "can return empty lists" $ do
      matches [Ruleset ".foo" [Rule "display" "none", Rule "position" "relative"],
               Ruleset ".bar" [Rule "display" "block"],
               Ruleset ".baz" [Rule "position" "absolute"]] == []
