{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Pages.SignIn where

import Lucid
import Data.Text (Text)

import Pages.Common

data SignInP = SignInP
  { title  :: !Text
  , errMsg :: !Banner
  }

pageData :: SignInP
pageData = SignInP
  { title = "Sign In"
  , errMsg = None
  }

instance ToHtml SignInP where
  toHtmlRaw = toHtml
  toHtml SignInP{..} = democracy_ $ div_ [class_ "content"] $
    section_ [class_ "container page"] $ do
      h1_ (toHtml title)
      showError_ errMsg
      form_ [action_ "/signin", method_ "POST"] $ do
        label_ [for_ "email"] "Email:"
        br_ []
        input_ [type_ "email", id_ "email", name_ "email", class_ "text-input"]
        br_ []
        label_ [for_ "password"] "Password:"
        br_ []
        input_ [type_ "password", id_ "password", name_ "password", class_ "text-input"]
        br_ []
        input_ [type_ "submit", value_ "Sign In"]
