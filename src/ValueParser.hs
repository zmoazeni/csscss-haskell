{-# LANGUAGE OverloadedStrings #-}

-- Parser is built from http://www.w3.org/TR/CSS/ as a reference

module ValueParser where

import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text.Lazy as AL
import Data.Text.Lazy (Text)
import Data.Text (unpack)
import qualified Data.Text as T (Text, length)
import Control.Applicative
import Prelude hiding (takeWhile)
import Data.Char
import Data.Foldable
import Control.Monad (liftM)

data Background = Background {getColor      :: Maybe Color,
                              getImage      :: Maybe Image,
                              getRepeat     :: Maybe Repeat,
                              getAttachment :: Maybe Attachment,
                              getPosition   :: Maybe Position} | InheritBackground
                deriving (Eq, Show, Ord)

data Color = Hex {getRGB :: String} |
             RGB {getR :: String, getG :: String, getB ::String} |
             RGBP {getRP :: String, getGP :: String, getBP :: String} |
             InheritColor
           deriving (Eq, Show, Ord)

data Image = Url {getUrl :: String } |
             NoneImage |
             InheritImage
           deriving (Eq, Show, Ord)

data Repeat = Repeat | RepeatX | RepeatY | NoRepeat | InheritRepeat
            deriving (Eq, Show, Ord)

data Attachment = Scroll | Fixed | InheritAttachment
            deriving (Eq, Show, Ord)

data LengthUnit = PX | EM | EX | IN | CM | MM | PT | PC
                deriving (Eq, Show, Ord)


data Length = Percent Number | Length {getLength :: Number, getLengthUnit :: LengthUnit}
           deriving (Eq, Show, Ord)


data HorizontalPoint = LeftPoint | RightPoint | HCenterPoint | HLength Length
           deriving (Eq, Show, Ord)

data VerticalPoint = TopPoint | BottomPoint | VCenterPoint | VLength Length
           deriving (Eq, Show, Ord)

data Position = Position (HorizontalPoint, Maybe VerticalPoint) | InheritPosition
              deriving (Eq, Show, Ord)

class Value a
instance Value Color
instance Value Image
instance Value Repeat
instance Value Attachment
instance Value Position
instance Value HorizontalPoint
instance Value VerticalPoint
instance Value LengthUnit

parseBackground :: Text -> Maybe Background
parseBackground s = AL.maybeResult $ AL.parse bg s

bg :: Parser Background
bg = inherit <|> longhand
  where
    inherit = do symbol "inherit"
                 endOfInput
                 return InheritBackground
    longhand = do color <- maybeTry bgColor
                  skipSpace
                  image <- maybeTry bgImage
                  skipSpace
                  repeat <- maybeTry bgRepeat
                  skipSpace
                  attachment <- maybeTry bgAttachment
                  skipSpace
                  position <- maybeTry bgPosition
                  return $ Background color image repeat attachment position

--
-- Utility
--
lexeme :: Parser a -> Parser a
lexeme p = p <* skipSpace

-- case insensitive
symbol :: T.Text -> Parser T.Text
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
comma = symbol "," >> return ()

literal :: Value v => T.Text -> v -> Parser v
literal s result = symbol s *> pure result

literalMap :: Value v => (T.Text, v) -> Parser v
literalMap (t, v) = literal t v

maybeTry :: Parser a -> Parser (Maybe a)
maybeTry p = Just <$> try (p) <|> return Nothing

--
-- Parsing Background Colors
--

bgColor :: Parser Color
bgColor = hexColor <|> rgbpColor <|> rgbColor <|> bgColorKeyword <|> inherit
  where inherit = literal "inherit" InheritColor

hexColor :: Parser Color
hexColor = do symbol "#"
              rawRGB <- takeWhile $ inClass "a-fA-F0-9"
              let rgb = expandRGB rawRGB (T.length rawRGB)
              return (Hex rgb)
  where expandRGB xs 3 = Prelude.concat $ map (\x -> [x, x]) (unpack xs)
        expandRGB xs _ = unpack xs

rgbColor :: Parser Color
rgbColor = do
  symbol "rgb"
  (r, g, b) <- rgbParams (takeWhile isNumber)
  return $ RGB r g b

rgbpColor :: Parser Color
rgbpColor = do
  symbol "rgb"
  (r, g, b) <- rgbParams percent
  return $ RGBP r g b
  where percent = takeWhile isNumber <* symbol "%"

rgbParams :: Parser T.Text -> Parser (String, String, String)
rgbParams p = parens $ do
  r <- p
  symbol ","
  g <- p
  symbol ","
  b <- p
  return (unpack r, unpack g, unpack b)

bgColorKeyword :: Parser Color
bgColorKeyword = asum $ fmap parseNamedColor namedColors
  where
    parseNamedColor (name, hexColor) = stringCI name *> pure (Hex hexColor)
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

--
-- Parsing Images
--
bgImage :: Parser Image
bgImage = bgImageUrl <|> none <|> inherit
  where none    = literal "none" NoneImage
        inherit = literal "inherit" InheritImage

bgImageUrl :: Parser Image
bgImageUrl = do
  symbol "url"
  url <- (singleQuoteUrl <|> doubleQuoteUrl <|> noQuoteUrl)
  return $ Url (unpack url)

  where singleQuoteUrl = parens $ singleQuotes innerUrl
        doubleQuoteUrl = parens $ doubleQuotes innerUrl
        noQuoteUrl     = parens innerUrl
        innerUrl       = takeWhile isUrl
        isUrl c        = isLetter c || isNumber c || inClass ":/?&." c

--
-- Parsing Repeat
--
bgRepeat :: Parser Repeat
bgRepeat = asum $ fmap literalMap repeats
  where
    repeats = [ ("repeat-x",  RepeatX)
              , ("repeat-y",  RepeatY)
              , ("no-repeat", NoRepeat)
              , ("repeat",    Repeat)
              , ("inherit",   InheritRepeat)
              ]

--
-- Parsing Attachment
--
bgAttachment :: Parser Attachment
bgAttachment = asum $ fmap literalMap attachments
  where
    attachments = [ ("scroll",  Scroll)
                  , ("fixed",   Fixed)
                  , ("inherit", InheritAttachment)
                  ]

--
-- Parsing Position
--
bgPosition :: Parser Position
bgPosition = points <|> inherit
  where
    points = do
      h <- asum $ (literalMap <$> hKeywords) ++ (h <$> [percentParser, lengthParser])
      v <- maybeTry . asum $ (literalMap <$> vKeywords) ++ (v <$> [percentParser, lengthParser])
      return $ Position (h, v)

    inherit = literalMap ("inherit", InheritPosition)

    h = liftM HLength
    v = liftM VLength

    percentParser = do p <- number
                       symbol "%"
                       return $ Percent p

    lengthParser = do len <- number
                      unit <- asum $ fmap literalMap units
                      return $ Length len unit

    units = [ ("px", PX)
            , ("em", EM)
            , ("ex", EX)
            , ("in", IN)
            , ("cm", CM)
            , ("mm", MM)
            , ("pt", PT)
            , ("pc", PC)]

    hKeywords = [ ("left",   LeftPoint)
                , ("right",  RightPoint)
                , ("center", HCenterPoint)]

    vKeywords = [ ("top",    TopPoint)
                , ("bottom", BottomPoint)
                , ("center", VCenterPoint)]
