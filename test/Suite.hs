{-# LANGUAGE TemplateHaskell #-}

module Main where

import Text.CSS.Shorthand
import TestHelper
import Test.HUnit

import Development.CSSCSS.RedundancyCalcTest as T1
import Development.CSSCSS.RulesetsTest       as T2

main = defaultMain [T1.tests, T2.tests]
