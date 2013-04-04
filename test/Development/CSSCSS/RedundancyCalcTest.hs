{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Development.CSSCSS.RedundancyCalcTest (tests) where

import TestHelper
import Test.HUnit
import Development.CSSCSS.Rulesets
import Development.CSSCSS.RedundancyCalc

main  = $(defaultMainGenerator)
tests = $(testGroupGenerator)

case_findMatches = do
  findMatches [
    Ruleset ".foo" [Rule "display" "none", Rule "position" "relative"],
    Ruleset ".bar" [Rule "position" "relative", Rule "display" "none"],
    Ruleset ".baz" [Rule "position" "relative"]
    ] 
  @?=
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

case_returnEmptyLists = do
  findMatches [
    Ruleset ".foo" [Rule "display" "none", Rule "position" "relative"],
    Ruleset ".bar" [Rule "display" "block"],
    Ruleset ".baz" [Rule "position" "absolute"]
    ] @?= []
