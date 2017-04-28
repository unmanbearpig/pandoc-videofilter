{-# LANGUAGE OverloadedStrings #-}

module VideoRenderer where

import Definitions
import Lucid
import Lucid.Base (makeAttribute, renderText)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)

videoUrl :: VideoId -> T.Text
videoUrl (VideoId service videoIdStr) =
  case service of
    Vimeo -> "https://player.vimeo.com/video/" <> videoId <> "?title=0&amp;portrait=0&amp;badge=0"
    YouTube -> "//www.youtube.com/embed/" <> videoId
  where videoId = T.pack videoIdStr

renderVideoEmbed :: Video -> LT.Text
renderVideoEmbed (Video vid dim) = renderText $ iframe_ (src_ (videoUrl vid) : attrs) ""
  where
    attrs = makeAttribute "frameborder" "0" : fullscreenAttrs ++ dimensionAttrs
    dimensionAttrs = videoDimensionsAttrs dim
    fullscreenAttrs = (`makeAttribute` "") <$> [ "webkitallowfullscreen", "mozallowfullscreen", "allowfullscreen" ]

videoDimensionsAttrs :: Maybe Dimensions -> [Attribute]
videoDimensionsAttrs d = do
  (w, h) <- catMaybes [d]
  [width_ (textShow w), height_ (textShow h)]
  where textShow = T.pack . show :: Show a => a -> T.Text
