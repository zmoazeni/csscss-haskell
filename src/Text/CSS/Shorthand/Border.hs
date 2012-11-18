{-# LANGUAGE OverloadedStrings #-}

module Text.CSS.Shorthand.Border (
    Border (..)
  , BorderWidth (..)
  , BorderWidths (..)

  , BorderStyle (..)
  , BorderStyles (..)

  , BorderColors (..)

  , parseBorder
  , borderParser

  , parseBorderWidths
  , borderWidthsParser

  , parseBorderStyles
  , borderStylesParser

  , parseBorderColors
  , borderColorsParser
) where

import Text.CSS.Shorthand.Utility
import Data.Attoparsec.Text hiding (take)
import qualified Data.Attoparsec.Text.Lazy as AL hiding (take)
import Data.Text.Lazy as L (Text)
import Data.Text (append, toLower, pack)
import Control.Applicative
import Control.Monad
import Data.Maybe
import Development.CSSCSS.Rulesets

data Border = Border {  getWidth :: Maybe BorderWidths
                      , getStyle :: Maybe BorderStyles
                      , getBorderColor :: Maybe BorderColors
                     } | InheritBorder
            deriving (Eq, Show, Ord)


data BorderWidth = Thin | Medium | Thick | WLength Length
                 deriving (Eq, Show, Ord)

data BorderWidths = BorderWidths {getTopWidth    :: BorderWidth,
                                  getRightWidth  :: BorderWidth,
                                  getBottomWidth :: BorderWidth,
                                  getLeftWidth   :: BorderWidth}
                    | InheritBorderWidth
                  deriving (Eq, Show, Ord)

data BorderStyle = None | Hidden | Dotted | Dashed | Solid | Double | Groove |
                   Ridge | Inset | Outset
                 deriving (Eq, Show, Ord)

data BorderStyles = BorderStyles {getTopStyle    :: BorderStyle,
                                  getRightStyle  :: BorderStyle,
                                  getBottomStyle :: BorderStyle,
                                  getLeftStyle   :: BorderStyle}
                    | InheritBorderStyle
                 deriving (Eq, Show, Ord)

data BorderColors = BorderColors {getTopColor    :: Color,
                                  getRightColor  :: Color,
                                  getBottomColor :: Color,
                                  getLeftColor   :: Color}
                    | InheritBorderColor
                 deriving (Eq, Show, Ord)
instance Value BorderWidth
instance Value BorderWidths
instance Value BorderStyle
instance Value BorderStyles

instance ShorthandProperty Border where
  getLonghandRules InheritBorder = []
  getLonghandRules (Border widths styles colors) = concat [longhandWidths, longhandStyles, longhandColors]
    where
      literalSides = ["top", "right", "bottom", "left"]

      longhandWidths = maybe [] (\(BorderWidths t r b l) ->
          zipWith (\side val -> width side val) literalSides [t, r, b, l]
        ) widths

      width side value = let value' = case value of
                                        WLength l -> pack (show l)
                                        _         -> toLower (pack (show value))
                         in Rule property value'
        where property = "border-" `append` side `append` "-width"

      longhandStyles = maybe [] (\(BorderStyles t r b l) ->
          zipWith (\side val -> style side val) literalSides [t, r, b, l]
        ) styles
      style side value = let value' = toLower (pack (show value))
                         in Rule property value'
        where property = "border-" `append` side `append` "-style"

      longhandColors = maybe [] (\(BorderColors t r b l) ->
          zipWith (\side val -> color' side val) literalSides [t, r, b, l]
        ) colors
      color' side value = let value' = pack (show value)
                         in Rule property value'
        where property = "border-" `append` side `append` "-color"


--
-- Parse Commands
--
parseBorder :: L.Text -> Maybe Border
parseBorder s = AL.maybeResult $ AL.parse borderParser s

parseBorderWidths :: L.Text -> Maybe BorderWidths
parseBorderWidths s = AL.maybeResult $ AL.parse borderWidthsParser s

parseBorderStyles :: L.Text -> Maybe BorderStyles
parseBorderStyles s = AL.maybeResult $ AL.parse borderStylesParser s

parseBorderColors :: L.Text -> Maybe BorderColors
parseBorderColors s = AL.maybeResult $ AL.parse borderColorsParser s
--
-- Parsers
--

borderParser :: Parser Border
borderParser = inherit <|> longhand
  where
    inherit = do symbol "inherit"
                 endOfInput
                 return InheritBorder

    inherit1 x = symbol "inherit" *> return x

    longhand = do widths <- maybeTry (borderWidths <|> (inherit1 InheritBorderWidth))
                  skipSpace
                  styles <- maybeTry (borderStyles <|> (inherit1 InheritBorderStyle))
                  skipSpace
                  colors <- maybeTry (((inherit1 InheritBorderColor) <* endOfInput) <|> borderColors)
                  if widths == Nothing && styles == Nothing && colors == Nothing
                    then empty
                    else return (Border widths styles colors)

    borderWidths = do
      w <- borderWidthParser
      return $ BorderWidths w w w w

    borderStyles = do
      s <- borderStyleParser
      return $ BorderStyles s s s s

    borderColors = do
      c <- color
      return $ BorderColors c c c c

borderWidthParser :: Parser BorderWidth
borderWidthParser = symbols [
    ("thin",   Thin)
  , ("medium", Medium)
  , ("thick",  Thick)] `mplus` (WLength <$> lengthParser)

borderWidthsParser :: Parser BorderWidths
borderWidthsParser = separate <|> inherit
  where
    inherit = do
      symbol "inherit"
      endOfInput
      return InheritBorderWidth

    separate = do
      ws <- many1 borderWidthParser
      let borderWidths = case ws of
                           (w1:[])          -> BorderWidths w1 w1 w1 w1
                           (w1:w2:[])       -> BorderWidths w1 w2 w1 w2
                           (w1:w2:w3:[])    -> BorderWidths w1 w2 w3 w2
                           (w1:w2:w3:w4:_)  -> BorderWidths w1 w2 w3 w4
      return borderWidths

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

borderStylesParser :: Parser BorderStyles
borderStylesParser = separate <|> inherit
  where
    inherit = do
      symbol "inherit"
      endOfInput
      return InheritBorderStyle
    separate = do
      styles <- many1 borderStyleParser
      let borderStyles = case styles of
                           (s1:[])          -> BorderStyles s1 s1 s1 s1
                           (s1:s2:[])       -> BorderStyles s1 s2 s1 s2
                           (s1:s2:s3:[])    -> BorderStyles s1 s2 s3 s2
                           (s1:s2:s3:s4:_)  -> BorderStyles s1 s2 s3 s4
      return borderStyles

borderColorsParser :: Parser BorderColors
borderColorsParser = inherit <|> separate
  where
    inherit = do
      symbol "inherit"
      endOfInput
      return InheritBorderColor

    separate = do
      colors <- many1 (skipSpace >> color)
      let borderColors = case colors of
                           (s1:[])          -> BorderColors s1 s1 s1 s1
                           (s1:s2:[])       -> BorderColors s1 s2 s1 s2
                           (s1:s2:s3:[])    -> BorderColors s1 s2 s3 s2
                           (s1:s2:s3:s4:_)  -> BorderColors s1 s2 s3 s4
      return borderColors
