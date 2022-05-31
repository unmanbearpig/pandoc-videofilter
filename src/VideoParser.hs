{-# LANGUAGE FlexibleContexts #-}

module VideoParser where

import Definitions
import Text.Parsec
import Data.Functor.Identity
import Data.Text

parse' :: Stream s Identity t => Parsec s () a -> s -> Maybe a
parse' p = either (const Nothing) Just . parse p ""

parseDimensions :: Text -> Maybe Dimensions
parseDimensions = either (const Nothing) Just . parse dimensionsParser ""

dimensionsParser :: Stream s m Char => ParsecT s u m (Int, Int)
dimensionsParser = do
  h <- many1 digit
  char 'x'
  w <- many1 digit
  return (read h, read w)

parseVideo :: Text -> Text -> Maybe Video
parseVideo url title = do
  vId <- parseVideoId url
  return $ Video vId $ parseDimensions title

parseVideoId :: Text -> Maybe VideoId
parseVideoId = parse' videoParser

videoParser :: Stream s m Char => ParsecT s u m VideoId
videoParser = vimeoParser <|> youTubeParser

vimeoParser :: Stream s m Char => ParsecT s u m VideoId
vimeoParser = do
  string "vimeo:"
  vId <- many digit
  return $ VideoId Vimeo vId

youTubeParser :: Stream s m Char => ParsecT s u m VideoId
youTubeParser = do
  string "youtube:"
  ytId <- many (alphaNum <|> char '_')
  return $ VideoId YouTube ytId
