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
    ".foo"@/["display"@:"none", "position"@:"relative"],
    ".bar"@/["position"@:"relative", "display"@:"none"],
    ".baz"@/["position"@:"relative"]
    ]
  @?= [
    ([".bar", ".foo"], ["position"@:"relative", "display"@:"none"]),
    ([".bar", ".baz", ".foo"], ["position"@:"relative"])
  ]

case_returnEmptyLists = do
  findMatches [
    ".foo"@/["display"@:"none", "position"@:"relative"],
    ".bar"@/["display"@:"block"],
    ".baz"@/["position"@:"absolute"]
    ] @?= []
