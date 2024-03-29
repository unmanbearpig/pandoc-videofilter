{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( process
    , inlineHtmlVideo
    ) where

import Definitions (Dimensions, Video(..))
import VideoParser (parseVideoId, parseDimensions)
import VideoRenderer (renderVideoEmbed)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as Text
import Data.List (uncons)
import Text.Pandoc.Definition (Inline(..), Format(..))
import Text.Pandoc.JSON (toJSONFilter)
import Text.Pandoc.Walk (query)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Environment (lookupEnv)
import Control.Applicative ((<|>))
import WrapperElement (WrapperElement(..), wrapWithElement, wrapperElementFromMaybeString)

extractDimensions :: [Inline] -> Maybe Dimensions
extractDimensions = fmap fst <$> uncons . mapMaybe (parseDimensions . query (\(Str s) -> s))

inlineHtmlVideo :: Maybe Dimensions -> WrapperElement -> Inline -> Maybe Inline
inlineHtmlVideo defaultDimensions wrapperEl (Image _ inlines (url, _)) =
  rawHtml . wrapWithElement wrapperEl . renderVideoEmbed . (`Video` dimensions) <$> parseVideoId url
  where rawHtml = RawInline (Format "html") . LT.toStrict
        dimensions = extractDimensions inlines <|> defaultDimensions
inlineHtmlVideo _ _ _ = Nothing

onFormat :: Format -> (a -> Maybe a) -> Maybe Format -> a -> a
onFormat format f maybeFormat x
  | maybeFormat == Just format = fromMaybe x $ f x
  | otherwise = x

process :: IO ()
process = do
  defaultDimensions <- (lookupEnv "VIDEO_DIMENSIONS")
  wrapperElementStr <- lookupEnv "VIDEO_WRAPPER_CSS_CLASS"
  toJSONFilter $ onFormat (Format "html") (inlineHtmlVideo (defaultDimensions >>= (parseDimensions . Text.pack)) (wrapperElementFromMaybeString wrapperElementStr))
