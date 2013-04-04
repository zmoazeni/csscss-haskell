{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Development.CSSCSS.RedundancyCalcTest (tests) where

import TestHelper
import Test.HUnit
import Development.CSSCSS.Rulesets
import Development.CSSCSS.RedundancyCalc

main  = $(defaultMainGenerator)
tests = $(testGroupGenerator)

sel @/ decs = Ruleset sel decs
property @: value = Declaration property value

case_findMatches = do
  findMatches [
    ".foo"@/["display"@:"none", "position"@:"relative"],
    ".bar"@/["position"@:"relative", "display"@:"none"],
    ".baz"@/["position"@:"relative"]
    ]
  @?=
    [((0, ".foo"@/["display"@:"none", "position"@:"relative"]), [
    Match 1 ".bar" ["display"@:"none", "position"@:"relative"],
    Match 2 ".baz" ["position"@:"relative"]]),

                   ((1, ".bar"@/["display"@:"none", "position"@:"relative"]), [
                   Match 0 ".foo" ["display"@:"none", "position"@:"relative"],
                   Match 2 ".baz" ["position"@:"relative"]]),

                   ((2, ".baz"@/["position"@:"relative"]), [
                   Match 0 ".foo" ["position"@:"relative"],
                   Match 1 ".bar" ["position"@:"relative"]])
                   ]

case_returnEmptyLists = do
  findMatches [
    ".foo"@/["display"@:"none", "position"@:"relative"],
    ".bar"@/["display"@:"block"],
    ".baz"@/["position"@:"absolute"]
    ] @?= []
