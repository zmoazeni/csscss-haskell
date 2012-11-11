{-# LANGUAGE OverloadedStrings #-}

module Text.CSS.Shorthand.Font (
    Font (..)
  , FontStyle (..)
  , FontVariant (..)
  , FontWeight (..)
  , FontSize (..)
  , LineHeight (..)
  , FontFamily (..)
  , SystemFont (..)

  , parseFont
  , fontParser

) where

import Text.CSS.Shorthand.Utility
import Data.Attoparsec.Text hiding (take)
import qualified Data.Attoparsec.Text.Lazy as AL hiding (take)
import Data.Text.Lazy as L (Text)
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import Data.Char

data Font = Font {  getFontStyle   :: Maybe FontStyle
                  , getFontVariant :: Maybe FontVariant
                  , getFontWeight  :: Maybe FontWeight
                  , getFontSize    :: Maybe FontSize
                  , getLineHeight  :: Maybe LineHeight
                  , getFontFamily  :: Maybe [FontFamily]
                  , getSystemFont  :: Maybe SystemFont
                  } | InheritFont
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

data FontFamily = FontName T.Text | SerifName | SansSerifName | CursiveName | FantasyName | MonospaceName | InheritFamily
                deriving (Eq, Show, Ord)

data SystemFont = CaptionFont | IconFont | MenuFont | MessageBoxFont | SmallCaptionFont | StatusBarFont
                deriving (Eq, Show, Ord)

instance Value FontStyle
instance Value FontVariant
instance Value FontWeight
instance Value FontSize
instance Value LineHeight
instance Value FontFamily
instance Value SystemFont

--
-- Parse Commands
--
parseFont :: L.Text -> Maybe Font
parseFont s = AL.maybeResult $ AL.parse fontParser s

--
-- Parsers
--

fontParser :: Parser Font
fontParser = inherit <|> longhand
  where
    inherit = do symbol "inherit"
                 endOfInput
                 return InheritFont

    longhand = do style <- maybeTry fontStyle
                  skipSpace
                  variant <- maybeTry fontVariant
                  skipSpace
                  weight <- maybeTry fontWeight
                  skipSpace
                  try (withFontSize $ Font style variant weight) <|> withoutFontSize

    withFontSize font = do size <- fontSize
                           skipSpace
                           lineHeight' <- maybeTry lineHeight
                           skipSpace
                           families <- maybeTry fontFamilies
                           return $ font (Just size) lineHeight' families Nothing

    withoutFontSize = do systemFont' <- systemFont
                         return $ Font Nothing Nothing Nothing Nothing Nothing Nothing (Just systemFont')


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

fontFamilies :: Parser [FontFamily]
fontFamilies = inherit <|> families
  where
    families = do f <- family'
                  maybeFS <- maybeTry $ many1 otherFamily
                  case maybeFS of
                    Just fs -> return (f:fs)
                    Nothing -> return [f]

    family' = generic <|> fontName
    fontName = liftM FontName (qQ <|> q <|> noQ)
    otherFamily = do skipSpace
                     symbol ","
                     family'

    generic = symbols [
        ("serif", SerifName)
      , ("san-serif", SansSerifName)
      , ("cursive", CursiveName)
      , ("fantasy", FantasyName)
      , ("monospace", MonospaceName)]

    qQ = doubleQuotes $ takeTill (== '"')
    q = singleQuotes $ takeTill (== '\'')
    noQ = takeTill (\x -> x == ',' || isSpace x)

    inherit = do f <- literalMap ("inherit", InheritFamily)
                 return [f]

systemFont :: Parser SystemFont
systemFont = symbols [
    ("caption",       CaptionFont)
  , ("icon",          IconFont)
  , ("menu",          MenuFont)
  , ("message-box",   MessageBoxFont)
  , ("small-caption", SmallCaptionFont)
  , ("status-bar",    StatusBarFont)]
