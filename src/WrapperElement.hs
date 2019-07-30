{-# LANGUAGE OverloadedStrings #-}

module WrapperElement where

import qualified Data.Text.Lazy as LT

data WrapperElement = DivWithClass String | NoWrapper

wrapperElementFromMaybeString :: Maybe String -> WrapperElement
wrapperElementFromMaybeString Nothing = NoWrapper
wrapperElementFromMaybeString (Just str) = DivWithClass str

wrapWithElement :: WrapperElement -> LT.Text -> LT.Text
wrapWithElement NoWrapper innerText = innerText
wrapWithElement (DivWithClass cssClass) innerText =
  "<div class=\"" <> (LT.pack cssClass) <> "\">" <> innerText <> "</div>"
