{-# LANGUAGE OverloadedStrings #-}

-- Parser is built from http://www.w3.org/TR/CSS/ as a reference

module Shorthand (
  module Shorthand.Background

  , HorizontalPoint (..)
  , VerticalPoint (..)
  , Position (..)
  , Length (..)
  , LengthUnit (..)
  , Color (..)
  , Image (..)
) where

import Shorthand.Background
import Shorthand.Utility