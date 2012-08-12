{-# LANGUAGE OverloadedStrings #-}

module ValueParser where

import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text.Lazy as AL
import Data.Text.Lazy (Text)
import Data.Text (unpack)
import qualified Data.Text as T (length)
import Control.Applicative
import Prelude hiding (takeWhile)
import Data.Char
import Data.Foldable

data Background = Background {getColor :: Maybe Color, getImage :: Maybe Image}

data Color = Hex {getRGB :: String} |
             RGB {getR :: String, getG :: String, getB ::String} |
             RGBP {getRP :: String, getGP :: String, getBP :: String} |
             InheritColor
           deriving (Eq, Show, Ord)

data Image = Url {getUrl :: String } |
             NoneImage |
             InheritImage
           deriving (Eq, Show, Ord)

parseBackground :: Text -> Maybe Background
parseBackground s = AL.maybeResult $ AL.parse bg s

bg :: Parser Background
bg = do color <- Just <$> try bgColor <|> return Nothing
        image <- Just <$> try bgImage <|> return Nothing
        return $ Background color image

bgColor :: Parser Color
bgColor = hexColor <|> rgbpColor <|> rgbColor <|> bgColorKeyword <|> inherit

hexColor :: Parser Color
hexColor = do string "#"
              rawRGB <- takeWhile $ inClass "a-fA-F0-9"
              let rgb = expandRGB rawRGB (T.length rawRGB)
              return (Hex rgb)
  where expandRGB xs 3 = concat $ map (\x -> [x, x]) (unpack xs)
        expandRGB xs _ = unpack xs



{-
These next few helper functions are pretty standard parsec functions
I'm sure someone already has them in some attoparsec library somewhere
-}
lexeme :: Parser a -> Parser a
lexeme p = p <* skipSpace

-- case insensitive
symbol :: Text -> Parser Text
symbol s = lexeme $ stringCI s

between :: Parser a -> Parser b -> Parser c -> Parser c
between open close p = do
  open
  x <- p
  close
  return x

parens :: Parser a -> Parser a
parens p = between (symbol "(") (symbol ")")

comma :: Parser ()
comma = symbol "," >> return ()




-- Is there a reason you don't use integers? or something other than strings?
rawInteger :: Parser String
rawInteger = liftM unpack $ takeWhile $ inClass "0-9"

integer :: Parser String 
integer = lexeme rawInteger

percent :: Parser String
percent = rawInteger <* symbol "%"

rgbColor :: Parser Color
rgbColor = do 
  symbol "rgb"
  (r, g, b) <- rgbParams integer
  return $ RGB r g b

rgbpColor :: Parser Color
rgbpColor = do 
  symbol "rgb"
  (r, g, b) <- rgbParams percent
  return $ RGBP r g b

rgbParams :: Parser (String, String, String)
rgbParams p = parens $ do
  r <- p
  symbol ","
  g <- p
  symbol ","
  b <- p
  return (r, g, b)

-- Often Using data, instead of functions, makes things cleaner
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

inherit :: Parser Color
inherit = stringCI "inherit" *> pure InheritColor

bgImage :: Parser Image
bgImage = do stringCI "url"
             skipSpace
             string "("
             skipSpace
             skipOptionalQuote
             url <- takeTill (inClass "\"' ")
             skipOptionalQuote
             skipSpace
             string ")"
             return $ Url (unpack url)
  where skipOptionalQuote = Just <$> try (skip (inClass "\"'")) <|> return Nothing