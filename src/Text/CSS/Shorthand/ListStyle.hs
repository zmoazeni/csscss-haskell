{-# LANGUAGE OverloadedStrings #-}

module Text.CSS.Shorthand.ListStyle (
    ListStyle (..)
  , ListStyleType (..)
  , ListStylePosition (..)

  , parseListStyle
  , listStyleParser

) where

import Text.CSS.Shorthand.Utility
import Data.Attoparsec.Text hiding (take)
import qualified Data.Attoparsec.Text.Lazy as AL hiding (take)
import Data.Text.Lazy as L (Text)
import Control.Applicative

data ListStyle = ListStyle { getListStyleType     :: Maybe ListStyleType
                           , getListStylePosition :: Maybe ListStylePosition
                           , getListStyleImage    :: Maybe Image
                           } | InheritListStyle
               deriving (Eq, Show, Ord)


data ListStyleType = DiscLSType | CircleLSType | SquareLSType | DecimalLSType | DecimalLeadingZeroLSType | LowerRomanLSType |
                     UpperRomanLSType | LowerGreekLSType | LowerLatinLSType | UpperLatinLSType | ArmenianLSType |
                     GeorgianLSType | LowerAlphaLSType | UpperAlphaLSType | NoneLSType | InheritLSType
                   deriving (Eq, Show, Ord)

data ListStylePosition = InsideLSPos | OutsideLSPos | InheritLSPos
                       deriving (Eq, Show, Ord)


instance Value ListStyleType
instance Value ListStylePosition
instance Value ListStyle

--
-- Parse Commands
--
parseListStyle :: L.Text -> Maybe ListStyle
parseListStyle s = AL.maybeResult $ AL.parse listStyleParser s

--
-- Parsers
--

listStyleParser :: Parser ListStyle
listStyleParser = inherit <|> longhand
  where
    inherit = do symbol "inherit"
                 endOfInput
                 return InheritListStyle

    longhand = do type' <- maybeTry listStyleType
                  skipSpace
                  position <- maybeTry listStylePosition
                  skipSpace
                  image <- maybeTry listStyleImage
                  return $ ListStyle type' position image


listStyleType :: Parser ListStyleType
listStyleType = symbols [
    ("disc", DiscLSType)
  , ("circle", CircleLSType)
  , ("square", SquareLSType)
  , ("decimal", DecimalLSType)
  , ("decimal-leading-zero", DecimalLeadingZeroLSType)
  , ("lower-roman", LowerRomanLSType)
  , ("upper-roman", UpperRomanLSType)
  , ("lower-greek", LowerGreekLSType)
  , ("lower-latin", LowerLatinLSType)
  , ("upper-latin", UpperLatinLSType)
  , ("armenian", ArmenianLSType)
  , ("georgian", GeorgianLSType)
  , ("lower-alpha", LowerAlphaLSType)
  , ("upper-alpha", UpperLatinLSType)
  , ("none", NoneLSType)
  , ("inherit", InheritLSType)]

listStylePosition :: Parser ListStylePosition
listStylePosition = symbols [
    ("inside",  InsideLSPos)
  , ("outside", OutsideLSPos)
  , ("inherit", InheritLSPos)]

listStyleImage :: Parser Image
listStyleImage = imageUrl <|> symbols [("none", NoneImage), ("inherit", InheritImage)]
