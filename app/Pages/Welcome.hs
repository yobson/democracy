{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, RecordWildCards #-}

module Pages.Welcome where

import Lucid
import Data.Text (Text)
import Pages.Common

import qualified Data.Text as T

data WelcomeP = WelcomeP
  { title   :: Text
  , message :: [Text]
  , errMsg  :: Banner
  }


instance ToHtml WelcomeP where
  toHtmlRaw = toHtml
  toHtml WelcomeP{..} = democracy_ $ div_ [class_ "content"] $
      section_ [class_ "container page"] $ do
        h1_ (toHtml title)
        showError_ errMsg
        mapM_ (p_ . toHtml) message


pageData :: WelcomeP
pageData = WelcomeP
  { title = "Democracy"
  , message =
    [ T.unwords [ "Democracy is a website for hosting online votes for any purpose." ]
    , T.unwords [ "At university and work, group decisions were taken using online polls,"
                , "often facebook group polls or google forms. There were a number of problems"
                , "The first is that on facebook, polls were not anonymous and the first past the"
                , "post voting system meant that similar options caused the splitting of the vote."
                , "The other is that google forms dump into a google sheet that can easily be"
                , "manipulated, and it's not fun when you don't trust your class reps!"
                ]
    , T.unwords [ "So I'm happy to introduce you to democracy!"
                , "This site will let you create online votes."
                ]
    , T.unwords [ "When creating a poll, you can choose the level of anonymity, the voting system,"
                , "and how the vote is conducted."
                ]
    ]
  , errMsg = None
  }
