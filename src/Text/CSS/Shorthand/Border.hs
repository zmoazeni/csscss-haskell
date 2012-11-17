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
import Data.Text (append, toLower, pack)
import Control.Applicative
import Control.Monad
import Data.Maybe
import Development.CSSCSS.Rulesets

data Border = Border {  getWidth :: Maybe BorderWidths
                      , getStyle :: Maybe BorderStyles
                     } | InheritBorder
            deriving (Eq, Show, Ord)


data BorderWidth = Thin | Medium | Thick | WLength Length
                 deriving (Eq, Show, Ord)

data BorderWidths = BorderWidths {getTopWidth    :: BorderWidth,
                                  getRightWidth  :: BorderWidth,
                                  getBottomWidth :: BorderWidth,
                                  getLeftWidth   :: BorderWidth}
                  deriving (Eq, Show, Ord)

data BorderStyle = None | Hidden | Dotted | Dashed | Solid | Double | Groove |
                   Ridge | Inset | Outset
                 deriving (Eq, Show, Ord)

data BorderStyles = BorderStyles {getTopStyle    :: BorderStyle,
                                  getRightStyle  :: BorderStyle,
                                  getBottomStyle :: BorderStyle,
                                  getLeftStyle   :: BorderStyle}
                 deriving (Eq, Show, Ord)

instance Value BorderWidth
instance Value BorderWidths
instance Value BorderStyle
instance Value BorderStyles

instance ShorthandProperty Border where
  getLonghandRules InheritBorder = []
  getLonghandRules (Border widths styles) = concat [longhandWidths, longhandStyles]
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
                  if widths == Nothing && styles == Nothing
                    then empty
                    else return (Border widths styles)


borderWidthParser :: Parser BorderWidth
borderWidthParser = symbols [
    ("thin",   Thin)
  , ("medium", Medium)
  , ("thick",  Thick)] `mplus` (WLength <$> lengthParser)

borderWidths :: Parser BorderWidths
borderWidths = do
  w <- borderWidthParser
  return $ BorderWidths w w w w


borderWidthsParser :: Parser BorderWidths
borderWidthsParser = do
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

borderStyles :: Parser BorderStyles
borderStyles = do
  s <- borderStyleParser
  return $ BorderStyles s s s s

borderStylesParser :: Parser BorderStyles
borderStylesParser = do
  styles <- many1 borderStyleParser
  let borderStyles = case styles of
                       (s1:[])          -> BorderStyles s1 s1 s1 s1
                       (s1:s2:[])       -> BorderStyles s1 s2 s1 s2
                       (s1:s2:s3:[])    -> BorderStyles s1 s2 s3 s2
                       (s1:s2:s3:s4:_)  -> BorderStyles s1 s2 s3 s4
  return borderStyles
