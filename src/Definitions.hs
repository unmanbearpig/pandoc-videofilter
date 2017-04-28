module Definitions where

type Dimensions = (Int, Int)
data VideoService = YouTube | Vimeo
data VideoId = VideoId VideoService String
data Video = Video VideoId (Maybe Dimensions)
