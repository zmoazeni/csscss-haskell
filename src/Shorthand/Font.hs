{-# LANGUAGE OverloadedStrings #-}

module Shorthand.Font (
    Font (..)
  , FontStyle (..)

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
                     }
            deriving (Eq, Show, Ord)


data FontStyle = Normal | Italic | Oblique | InheritStyle
           deriving (Eq, Show, Ord)

instance Value FontStyle


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
                  -- skipSpace
                  -- style <- maybeTry borderStyle
                  return $ Font style


fontStyle :: Parser FontStyle
fontStyle = asum $ literalMap <$> keywords
  where
    keywords = [ ("normal",  Normal)
               , ("italic",  Italic)
               , ("oblique", Oblique)
               , ("inherit", InheritStyle)]