{-# LANGUAGE OverloadedStrings #-}

module Text.CSS.Shorthand.Border (
    Border (..)
  , BorderWidth (..)
  , BorderWidths (..)

  , BorderStyle (..)
  , BorderStyles (..)

  , parseBorder
  , borderParser

  , parseBorderWidths
  , borderWidthsParser

  , parseBorderStyles
  , borderStylesParser
) where

import Text.CSS.Shorthand.Utility
import Data.Attoparsec.Text hiding (take)
import qualified Data.Attoparsec.Text.Lazy as AL hiding (take)
import Data.Text.Lazy as L (Text)
import Control.Applicative
import Control.Monad

data Border = Border {  getWidth :: Maybe BorderWidths
                      , getStyle :: Maybe BorderStyles
                     } | InheritBorder
            deriving (Eq, Show, Ord)


data BorderWidth = Thin | Medium | Thick | WLength Length
                 deriving (Eq, Show, Ord)

data BorderWidths = BorderWidths {getTopWidth    :: Maybe BorderWidth,
                                  getRightWidth  :: Maybe BorderWidth,
                                  getBottomWidth :: Maybe BorderWidth,
                                  getLeftWidth   :: Maybe BorderWidth}
                  deriving (Eq, Show, Ord)

data BorderStyle = None | Hidden | Dotted | Dashed | Solid | Double | Groove |
                   Ridge | Inset | Outset
                 deriving (Eq, Show, Ord)

data BorderStyles = BorderStyles {getTopStyle    :: Maybe BorderStyle,
                                  getRightStyle  :: Maybe BorderStyle,
                                  getBottomStyle :: Maybe BorderStyle,
                                  getLeftStyle   :: Maybe BorderStyle}
                 deriving (Eq, Show, Ord)

instance Value BorderWidth
instance Value BorderWidths
instance Value BorderStyle
instance Value BorderStyles

--
-- Parse Commands
--
parseBorder :: L.Text -> Maybe Border
parseBorder s = AL.maybeResult $ AL.parse borderParser s

parseBorderWidths :: L.Text -> Maybe BorderWidths
parseBorderWidths s = AL.maybeResult $ AL.parse borderWidthsParser s

parseBorderStyles :: L.Text -> Maybe BorderStyles
parseBorderStyles s = AL.maybeResult $ AL.parse borderStylesParser s

--
-- Parsers
--

borderParser :: Parser Border
borderParser = inherit <|> longhand
  where
    inherit = do symbol "inherit"
                 endOfInput
                 return InheritBorder
    longhand = do widths <- maybeTry borderWidths
                  skipSpace
                  styles <- maybeTry borderStyles
                  return $ Border widths styles


borderWidthParser :: Parser BorderWidth
borderWidthParser = symbols [
    ("thin",   Thin)
  , ("medium", Medium)
  , ("thick",  Thick)] `mplus` (WLength <$> lengthParser)

borderWidths :: Parser BorderWidths
borderWidths = do w <- borderWidthParser
                  return $ BorderWidths (Just w) (Just w) (Just w) (Just w)


borderWidthsParser :: Parser BorderWidths
borderWidthsParser = do ws <- many1 borderWidthParser
                        let (w1:w2:w3:w4:_) = pad (map Just ws)
                        return $ BorderWidths w1 w2 w3 w4
  where
    pad ws = take 4 $ ws ++ repeat Nothing

borderStyleParser :: Parser BorderStyle
borderStyleParser = symbols [
    ("none",   None)
  , ("hidden", Hidden)
  , ("dotted", Dotted)
  , ("dashed", Dashed)
  , ("solid",  Solid)
  , ("double", Double)
  , ("groove", Groove)
  , ("ridge",  Ridge)
  , ("inset",  Inset)
  , ("outset", Outset)]

borderStyles :: Parser BorderStyles
borderStyles = do s <- borderStyleParser
                  return $ BorderStyles (Just s) (Just s) (Just s) (Just s)

borderStylesParser :: Parser BorderStyles
borderStylesParser = do ws <- many1 borderStyleParser
                        let (w1:w2:w3:w4:_) = pad (map Just ws)
                        return $ BorderStyles w1 w2 w3 w4
  where
    pad ws = take 4 $ ws ++ repeat Nothing

