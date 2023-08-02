{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, RecordWildCards #-}

module Pages.Register where

import Data.Text (Text)
import Lucid

import Pages.Common

data RegisterP = RegisterP
  { title    :: !Text
  , message  :: !Text
  , errorMsg :: Banner
  }


pageData :: RegisterP
pageData = RegisterP
  { title = "Register"
  , message = "The data provided is only used to link you to the votes you have created. It is used for no other purposes!"
  , errorMsg = None
  }

instance ToHtml RegisterP where
  toHtmlRaw = toHtml
  toHtml RegisterP{..} = democracy_ $ div_ [class_ "content"] $
    section_ [class_ "container page"] $ do
      h1_ (toHtml title)
      p_ (toHtml message)
      showError_ errorMsg
      form_ [action_ "/register", method_ "POST"] $ do
        label_ [for_ "email"] "Email:"
        br_ []
        input_ [type_ "email", id_ "email", name_ "email", class_ "text-input"]
        br_ []
        label_ [for_ "password1"] "Password:"
        br_ []
        input_ [type_ "password", id_ "password1", name_ "password1", class_ "text-input"]
        br_ []
        label_ [for_ "password2"] "Repeat Password:"
        br_ []
        input_ [type_ "password", id_ "password2", name_ "password2", class_ "text-input"]
        br_ []
        input_ [type_ "submit", value_ "Register!"]


