{-# LANGUAGE OverloadedStrings #-}

module Shorthand.Font (
    Font (..)
  , FontStyle (..)
  , FontVariant (..)
  , FontWeight (..)
  , FontSize (..)
  , LineHeight (..)

  , parseFont
  , fontParser

) where

import Shorthand.Utility
import Data.Attoparsec.Text hiding (take)
import qualified Data.Attoparsec.Text.Lazy as AL hiding (take)
import Data.Text.Lazy as L (Text)
import Control.Applicative
import Data.Foldable
import Control.Monad

data Font = Font {  getFontStyle   :: Maybe FontStyle
                  , getFontVariant :: Maybe FontVariant
                  , getFontWeight  :: Maybe FontWeight
                  , getFontSize    :: FontSize
                  , getLineHeight  :: Maybe LineHeight
                  }
          deriving (Eq, Show, Ord)


data FontStyle = NormalStyle | ItalicStyle | ObliqueStyle | InheritStyle
               deriving (Eq, Show, Ord)

data FontVariant = NormalVariant | SmallCapsVariant | InheritVariant
                 deriving (Eq, Show, Ord)

data FontWeight = NormalWeight | BoldWeight | BolderWeight | LighterWeight | NumberWeight Number | InheritWeight
                deriving (Eq, Show, Ord)

data FontSize = XXSmallSize | XSmallSize | SmallSize | MediumSize | LargeSize | XLargeSize | XXLargeSize | LargerSize | SmallerSize | LengthSize Length | PercentSize Percent | InheritSize
              deriving (Eq, Show, Ord)

data LineHeight = NormalLH | NumberLH Number | LengthLH Length | PercentLH Percent | InheritLH
                  deriving (Eq, Show, Ord)


instance Value FontStyle
instance Value FontVariant
instance Value FontWeight
instance Value FontSize
instance Value LineHeight

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
                  skipSpace
                  weight <- maybeTry fontWeight
                  skipSpace
                  size <- fontSize
                  skipSpace
                  lineHeight' <- maybeTry lineHeight
                  return $ Font style variant weight size lineHeight'


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

fontWeight :: Parser FontWeight
fontWeight = symbols [
    ("normal",  NormalWeight)
  , ("bold",    BoldWeight)
  , ("bolder",  BolderWeight)
  , ("lighter", LighterWeight)
  , ("inherit", InheritWeight)]
             `mplus` numberWeight

  where numberWeight = do n <- number
                          space
                          return $ NumberWeight n

fontSize :: Parser FontSize
fontSize = symbols [
    ("xx-small", XXSmallSize)
  , ("x-small",  XSmallSize)
  , ("small",    SmallSize)
  , ("medium",   MediumSize)
  , ("larger",   LargerSize)
  , ("smaller",  SmallerSize)
  , ("large",    LargeSize)
  , ("x-large",  XLargeSize)
  , ("xx-large", XXLargeSize)
  , ("inherit",  InheritSize)]
           `mplus` len
           `mplus` pct

  where
    len = LengthSize <$> lengthParser
    pct = PercentSize <$> percentParser

lineHeight :: Parser LineHeight
lineHeight = do symbol "/"
                symbols [
                    ("normal", NormalLH)
                  , ("inherit", InheritLH)]
                  <|> len <|> pct <|> num

  where
    len = LengthLH <$> lengthParser
    pct = PercentLH <$> percentParser
    num = NumberLH <$> number
