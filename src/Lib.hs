{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( process
    ) where

import VideoParser
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.List (uncons)
import Text.Pandoc.JSON
import Text.Pandoc.Walk (query)
import Lucid
import Lucid.Base (makeAttribute, renderText)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)

videoUrl :: VideoId -> T.Text
videoUrl (VideoId service videoIdStr) =
  case service of
    Vimeo -> "https://player.vimeo.com/video/" <> videoId <> "?title=0&amp;portrait=0&amp;badge=0"
    YouTube -> "//www.youtube.com/embed/" <> videoId
  where videoId = T.pack videoIdStr

videoDimensionsAttrs :: Maybe Dimensions -> [Attribute]
videoDimensionsAttrs d = do
  (w, h) <- catMaybes [d]
  [width_ (textShow w), height_ (textShow h)]
  where textShow = T.pack . show :: Show a => a -> T.Text

renderVideoEmbed :: Video -> LT.Text
renderVideoEmbed (Video vid dim) = renderText $ iframe_ (src_ (videoUrl vid) : attrs) ""
  where
    attrs = makeAttribute "frameborder" "0" : fullscreenAttrs ++ dimensionAttrs
    dimensionAttrs = videoDimensionsAttrs dim
    fullscreenAttrs = (`makeAttribute` "") <$> [ "webkitallowfullscreen", "mozallowfullscreen", "allowfullscreen" ]

extractDimensions :: [Inline] -> Maybe Dimensions
extractDimensions = fmap fst <$> uncons . mapMaybe (parseDimensions . query (\(Str s) -> s))

inlineHtmlVideo :: Inline -> Maybe Inline
inlineHtmlVideo (Image _ inlines (url, _)) =
  rawHtml . renderVideoEmbed . (`Video` extractDimensions inlines) <$> parseVideoId url
  where rawHtml = RawInline (Format "html") . LT.unpack
inlineHtmlVideo _ = Nothing

onFormat :: Format -> (a -> Maybe a) -> Maybe Format -> a -> a
onFormat format f maybeFormat x
  | maybeFormat == Just format = fromMaybe x $ f x
  | otherwise = x

process :: IO ()
process = toJSONFilter $ onFormat (Format "html") inlineHtmlVideo
