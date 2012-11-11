{-# LANGUAGE OverloadedStrings #-}

module Development.CSSCSS.RedundancyCalcSpec where

import Test.Hspec
import Development.CSSCSS.Rulesets
import Development.CSSCSS.RedundancyCalc

main = hspec spec

spec = describe "redundancy calc" $ do
  describe "findMatches" $ do
    it "finds shared rules" $ do
      findMatches [Ruleset ".foo" [Rule "display" "none", Rule "position" "relative"],
                   Ruleset ".bar" [Rule "position" "relative", Rule "display" "none"],
                   Ruleset ".baz" [Rule "position" "relative"]]
                  ==
                  [((0, Ruleset ".foo" [Rule "display" "none", Rule "position" "relative"]), [
                      Match 1 ".bar" [Rule "display" "none", Rule "position" "relative"],
                      Match 2 ".baz" [Rule "position" "relative"]]),

                   ((1, Ruleset ".bar" [Rule "display" "none", Rule "position" "relative"]), [
                      Match 0 ".foo" [Rule "display" "none", Rule "position" "relative"],
                      Match 2 ".baz" [Rule "position" "relative"]]),

                   ((2, Ruleset ".baz" [Rule "position" "relative"]), [
                      Match 0 ".foo" [Rule "position" "relative"],
                      Match 1 ".bar" [Rule "position" "relative"]])
                  ]


    it "can return empty lists" $ do
      findMatches [Ruleset ".foo" [Rule "display" "none", Rule "position" "relative"],
                   Ruleset ".bar" [Rule "display" "block"],
                   Ruleset ".baz" [Rule "position" "absolute"]] == []
