{-# LANGUAGE OverloadedStrings #-}

module RedundancyCalcSpec where

import Test.Hspec
import Rulesets
import RedundancyCalc

main = hspec spec

spec = describe "redundancy calc" $ do
  describe "100% matches" $ do
    it "finds them (example1)" $ do
      matches [Ruleset ".foo" [Rule "display" "none", Rule "position" "relative"],
               Ruleset ".bar" [Rule "position" "relative", Rule "display" "none"],
               Ruleset ".baz" [Rule "position" "relative"]] == [MatchResult [".bar", ".foo"] [Rule "display" "none", Rule "position" "relative"]]

    it "can return empty lists" $ do
      matches [Ruleset ".foo" [Rule "display" "none", Rule "position" "relative"],
               Ruleset ".bar" [Rule "display" "none"],
               Ruleset ".baz" [Rule "position" "relative"]] == []
