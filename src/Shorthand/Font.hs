{-# LANGUAGE OverloadedStrings #-}

module Shorthand.Font (
    Font (..)
  , FontStyle (..)
  , FontVariant (..)

  , parseFont
  , fontParser

) where

import Shorthand.Utility
import Data.Attoparsec.Text hiding (take)
import qualified Data.Attoparsec.Text.Lazy as AL hiding (take)
import Data.Text.Lazy as L (Text)
import Control.Applicative
import Data.Foldable

data Font = Font {  getFontStyle :: Maybe FontStyle
                  , getFontVariant :: Maybe FontVariant
                  }
          deriving (Eq, Show, Ord)


data FontStyle = NormalStyle | ItalicStyle | ObliqueStyle | InheritStyle
               deriving (Eq, Show, Ord)

data FontVariant = NormalVariant | SmallCapsVariant | InheritVariant
                 deriving (Eq, Show, Ord)

instance Value FontStyle
instance Value FontVariant


--
-- Parse Commands
--
parseFont :: L.Text -> Maybe Font
parseFont s = AL.maybeResult $ AL.parse fontParser s

--
-- Parsers
--

fontParser :: Parser Font
fontParser = longhand
  where
    longhand = do style <- maybeTry fontStyle
                  skipSpace
                  variant <- maybeTry fontVariant
                  return $ Font style variant


fontStyle :: Parser FontStyle
fontStyle = symbols [
    ("normal",  NormalStyle)
  , ("italic",  ItalicStyle)
  , ("oblique", ObliqueStyle)
  , ("inherit", InheritStyle)]

fontVariant :: Parser FontVariant
fontVariant = symbols [
    ("normal",     NormalVariant)
  , ("small-caps", SmallCapsVariant)
  , ("inherit",    InheritVariant)]
