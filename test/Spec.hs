{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Text.Pandoc.Definition (Format(..), Inline(..), Attr(), nullAttr)
import WrapperElement (WrapperElement(..))
import Lib (inlineHtmlVideo)

main :: IO ()
main = hspec $ do
  describe "inlneHTMLVideo" $ do
    it "returns Nothing for non-image tag" $ do
      inlineHtmlVideo Nothing NoWrapper (Str "hello") `shouldBe` (Nothing)
    it "returns Vimeo iframe for correctly formatted image tag" $ do
      inlineHtmlVideo Nothing NoWrapper (Image nullAttr [] ("vimeo:123", ""))
        `shouldBe` (Just $
                     RawInline (Format "html")
                     "<iframe src=\"https://player.vimeo.com/video/123?title=0&amp;amp;portrait=0&amp;amp;badge=0\" frameborder=\"0\" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>")
    it "uses provided default dimensions" $ do
      inlineHtmlVideo (Just (10, 20)) NoWrapper (Image nullAttr [] ("vimeo:123", ""))
        `shouldBe` (Just $
                     RawInline (Format "html")
                     "<iframe src=\"https://player.vimeo.com/video/123?title=0&amp;amp;portrait=0&amp;amp;badge=0\" frameborder=\"0\" webkitallowfullscreen mozallowfullscreen allowfullscreen width=\"10\" height=\"20\"></iframe>")
    it "overrides default dimensions" $ do
      inlineHtmlVideo Nothing NoWrapper (Image nullAttr [(Str "40x50")] ("vimeo:123", ""))
        `shouldBe` (Just $
                     RawInline (Format "html")
                     "<iframe src=\"https://player.vimeo.com/video/123?title=0&amp;amp;portrait=0&amp;amp;badge=0\" frameborder=\"0\" webkitallowfullscreen mozallowfullscreen allowfullscreen width=\"40\" height=\"50\"></iframe>")
    it "wraps it in a div" $ do
      inlineHtmlVideo Nothing (DivWithClass "class1 class2") (Image nullAttr [] ("vimeo:123", ""))
        `shouldBe` (Just $
                     RawInline (Format "html")
                     "<div class=\"class1 class2\"><iframe src=\"https://player.vimeo.com/video/123?title=0&amp;amp;portrait=0&amp;amp;badge=0\" frameborder=\"0\" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe></div>")
    it "doesn't wrap when no video" $ do
      inlineHtmlVideo Nothing (DivWithClass "testclass") (Str "hello") `shouldBe` (Nothing)
