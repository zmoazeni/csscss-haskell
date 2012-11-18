{-# LANGUAGE OverloadedStrings #-}

module Text.CSS.Shorthand.Utility
where

import Data.Attoparsec.Text
import Control.Applicative
import Data.Text (Text, unpack)
import Data.Text as T (length)
import Data.Foldable
import Prelude hiding (takeWhile)
import Data.Char
import Control.Monad

import Development.CSSCSS.Rulesets

data Color = Hex {getRGB :: String} |
             RGB {getR :: String, getG :: String, getB ::String} |
             RGBP {getRP :: String, getGP :: String, getBP :: String} |
             InheritColor
           deriving (Eq, Ord)

instance Show Color where
  show InheritColor = "InheritColor"
  show (Hex s) = "#" ++ s
  show (RGB r g b) = "rgb(" ++ r ++ ", " ++ g ++ ", " ++ b ++ ")"
  show (RGBP r g b) = "rgb(" ++ r ++ "%, " ++ g ++ "%, " ++ b ++ "%)"

data Image = Url {getUrl :: String } |
             NoneImage |
             InheritImage
           deriving (Eq, Show, Ord)

data LengthUnit = PX | EM | EX | IN | CM | MM | PT | PC
                deriving (Eq, Show, Ord)


data Percent = Percent Number
             deriving (Eq, Show, Ord)

data Length = Length {getLength :: Number, getLengthUnit :: LengthUnit}
            deriving (Eq, Ord)

instance Show Length where
  show (Length n unit) = show n ++ unitS
    where unitS = map toLower (show unit)


data HorizontalPoint = LeftPoint | RightPoint | HCenterPoint | HLength Length | HPercent Percent
           deriving (Eq, Show, Ord)

data VerticalPoint = TopPoint | BottomPoint | VCenterPoint | VLength Length | VPercent Percent
           deriving (Eq, Show, Ord)

data Position = Position (HorizontalPoint, Maybe VerticalPoint) | InheritPosition
              deriving (Eq, Show, Ord)

class Value a
instance Value Color
instance Value Image
instance Value Position
instance Value HorizontalPoint
instance Value VerticalPoint
instance Value LengthUnit

class ShorthandProperty a where
  getLonghandRules :: a -> [Rule]

lexeme :: Parser a -> Parser a
lexeme p = p <* skipSpace

-- case insensitive
symbol :: Text -> Parser Text
symbol s = lexeme $ stringCI s

between :: Parser a -> Parser b -> Parser c -> Parser c
between open close p = do
  open
  skipSpace
  x <- p
  skipSpace
  close
  return x

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

singleQuotes :: Parser a -> Parser a
singleQuotes = between (symbol "'") (symbol "'")

doubleQuotes :: Parser a -> Parser a
doubleQuotes = between (symbol "\"") (symbol "\"")

comma :: Parser ()
comma = void(symbol ",")

literal :: Value v => Text -> v -> Parser v
literal s result = symbol s *> pure result

literalMap :: Value v => (Text, v) -> Parser v
literalMap (t, v) = literal t v

symbols :: Value v => [(Text, v)] -> Parser v
symbols keywords = asum $ literalMap <$> keywords


maybeTry :: Parser a -> Parser (Maybe a)
maybeTry p = Just <$> try p <|> return Nothing

percentParser :: Parser Percent
percentParser = do p <- number
                   symbol "%"
                   return $ Percent p

lengthParser :: Parser Length
lengthParser = do len <- number
                  unit <- asum $ fmap literalMap units
                  return $ Length len unit
  where
    units = [ ("px", PX)
            , ("em", EM)
            , ("ex", EX)
            , ("in", IN)
            , ("cm", CM)
            , ("mm", MM)
            , ("pt", PT)
            , ("pc", PC)]

imageUrl :: Parser Image
imageUrl = do
  symbol "url"
  url <- singleQuoteUrl <|> doubleQuoteUrl <|> noQuoteUrl
  return $ Url (unpack url)

  where singleQuoteUrl = parens $ singleQuotes innerUrl
        doubleQuoteUrl = parens $ doubleQuotes innerUrl
        noQuoteUrl     = parens innerUrl
        innerUrl       = takeWhile isUrl
        isUrl c        = isLetter c || isNumber c || inClass ":/?&." c

color :: Parser Color
color = hexColor <|> rgbpColor <|> rgbColor <|> keyword <|> inherit
  where
    inherit = literal "inherit" InheritColor

    hexColor = do symbol "#"
                  rawRGB <- takeWhile $ inClass "a-fA-F0-9"
                  let rgb = expandRGB rawRGB (T.length rawRGB)
                  return (Hex rgb)
      where expandRGB xs 3 = Prelude.concatMap (\x -> [x, x]) (unpack xs)
            expandRGB xs _ = unpack xs

    rgbColor = do
      symbol "rgb"
      (r, g, b) <- rgbParams (takeWhile isNumber)
      return $ RGB r g b

    rgbpColor = do
      symbol "rgb"
      (r, g, b) <- rgbParams percent
      return $ RGBP r g b
      where percent = takeWhile isNumber <* symbol "%"

    rgbParams p = parens $ do
      r <- p
      symbol ","
      g <- p
      symbol ","
      b <- p
      return (unpack r, unpack g, unpack b)

    keyword = asum $ fmap parseNamedColor namedColors
      where
        parseNamedColor (name, hexColor') = stringCI name *> pure (Hex hexColor')
        namedColors = [ ("black",   "000000")
                      , ("silver",  "c0c0c0")
                      , ("gray",    "808080")
                      , ("white",   "ffffff")
                      , ("maroon",  "800000")
                      , ("red",     "ff0000")
                      , ("purple",  "800080")
                      , ("fuchsia", "ff00ff")
                      , ("green",   "008000")
                      , ("lime",    "00ff00")
                      , ("olive",   "808000")
                      , ("yellow",  "ffff00")
                      , ("navy",    "000080")
                      , ("blue",    "0000ff")
                      , ("teal",    "008080")
                      , ("aqua",    "00ffff")
                      ]
