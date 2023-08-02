{-# LANGUAGE OverloadedStrings #-}

module Pages.Common where

import Lucid
import Data.Text (Text, pack)
import Data.String


top_ :: (Monad m) => HtmlT m ()
top_ = html_ $
  head_ $ do
    title_ "Democracy -- Online Poll Builder"
    link_ [rel_ "stylesheet", href_ "static/style.css"]


bottom_ :: (Monad m) => HtmlT m ()
bottom_ = return ()

navBar_ :: (Monad m) => HtmlT m ()
navBar_ = ul_ $ do
  li_ $ a_ [href_ "/"]         "Home"
  li_ $ a_ [href_ "/register"] "Register"
  li_ $ a_ [href_ "/signin"]   "Sign In"

navBarLI_ :: (Monad m) => HtmlT m ()
navBarLI_ = ul_ $ do
  li_ $ a_ [href_ "/"]             "Home"
  li_ $ a_ [href_ "/create"]       "New Election"
  li_ $ a_ [href_ "/my-elections"] "My Elections"
  li_ $ a_ [href_ "/signout"]      "Sign Out"

democracy_ :: (Monad m) => HtmlT m () -> HtmlT m ()
democracy_ b = do
  top_
  body_ $ do
    navBar_
    b
  bottom_

democracyLI_ :: (Monad m) => HtmlT m () -> HtmlT m ()
democracyLI_ b = do
  top_
  body_ $ do
    navBarLI_
    b
  bottom_

data Banner = None
            | Red !Text
            | Green !Text

instance IsString Banner where
  fromString ""       = None
  fromString ('!':xs) = Red $ pack xs
  fromString xs       = Green $ pack xs

showError_ :: Monad m => Banner -> HtmlT m ()
showError_ None      = return ()
showError_ (Red msg) = div_ [class_ "alert bad"] $ do
  span_ [class_ "closebtn bad", onclick_ "this.parentElement.style.display='none';"] "X"
  toHtml msg
showError_ (Green msg) = div_ [class_ "alert good"] $ do
  span_ [class_ "closebtn good", onclick_ "this.parentElement.style.display='none';"] "X"
  toHtml msg
