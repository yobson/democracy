{-# LANGUAGE RecordWildCards, OverloadedStrings, ExtendedDefaultRules #-}

module Pages.Create where

import Data.Text (Text)
import Lucid

import Pages.Common
import Api.Types

data CreateP = CreateP
  { title  :: Text
  , user   :: User
  , errMsg :: Banner
  }

instance ToHtml CreateP where
  toHtmlRaw = toHtml
  toHtml CreateP{..} = democracyLI_ user $
    section_ [class_ "container page"] $ do
      h1_ (toHtml title)
      showError_ errMsg
      form_ [action_ "/create", method_ "POST"] "Hello World"
      script_ [src_ "static/create.js"] ("" :: Html ())


pageData :: CreateP
pageData = CreateP
  { title = "Create Election"
  , errMsg = None
  , user  = User ""
  }
