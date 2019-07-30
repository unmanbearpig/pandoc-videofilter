import Test.Hspec
import Text.Pandoc.Definition (Format(..), Inline(..), Attr(), nullAttr)
import Lib (inlineHtmlVideo)

main :: IO ()
main = hspec $ do
  describe "inlneHTMLVideo" $ do
    it "returns Nothing for non-image tag" $ do
      inlineHtmlVideo Nothing (Str "hello") `shouldBe` (Nothing)
    it "returns Vimeo iframe for correctly formatted image tag" $ do
      inlineHtmlVideo Nothing (Image nullAttr [] ("vimeo:123", ""))
        `shouldBe` (Just $
                     RawInline (Format "html")
                     "<iframe allowfullscreen mozallowfullscreen frameborder=\"0\" src=\"https://player.vimeo.com/video/123?title=0&amp;amp;portrait=0&amp;amp;badge=0\" webkitallowfullscreen></iframe>")
    it "uses provided default dimensions" $ do
      inlineHtmlVideo (Just (10, 20)) (Image nullAttr [] ("vimeo:123", ""))
        `shouldBe` (Just $
                     RawInline (Format "html")
                     "<iframe height=\"20\" width=\"10\" allowfullscreen mozallowfullscreen frameborder=\"0\" src=\"https://player.vimeo.com/video/123?title=0&amp;amp;portrait=0&amp;amp;badge=0\" webkitallowfullscreen></iframe>")
    it "overrides default dimensions" $ do
      inlineHtmlVideo Nothing (Image nullAttr [(Str "40x50")] ("vimeo:123", ""))
        `shouldBe` (Just $
                     RawInline (Format "html")
                     "<iframe height=\"50\" width=\"40\" allowfullscreen mozallowfullscreen frameborder=\"0\" src=\"https://player.vimeo.com/video/123?title=0&amp;amp;portrait=0&amp;amp;badge=0\" webkitallowfullscreen></iframe>")
