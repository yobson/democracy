{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, RecordWildCards #-}
{-# OPTIONS -Wno-type-defaults #-}

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
  toHtml CreateP{..} = democracyLI_ user $ do
    section_ [class_ "container page"] $ do
      h1_ (toHtml title)
      showError_ errMsg
      form_ [action_ "/create", method_ "POST"] $ do
        elecType_
        singleSys_
        multiSys_
        fptpSing_

        div_ [id_ "buttons", class_ "buttons"] $ do
          button_ [type_ "button", id_ "previousBtn", hidden_ "true"] "Previous"
          button_ [type_ "button", class_ "next", id_ "nextBtn"] "Next"
    script_ [src_ "static/create.js"] ""

elecType_ :: Monad m => HtmlT m ()
elecType_ = div_ [id_ "elecType"] $ do
  h2_ "Select Election Type"
  label_ [for_ "singMulti"] "Are you selecting one person/outcome, or electing a group of people/outcomes?"
  br_ []
  select_ [name_ "singMulti", id_ "singMulti"] $ do
    option_ [value_ "single"] "Single person/outcome"
    option_ [value_ "multi"] "Multiple person/outcome"

singleSys_ :: (Monad m) => HtmlT m ()
singleSys_ = div_ [id_ "singleSys", hidden_ "true"] $ do
  h2_ "Election System (Single winner)"
  h3_ "First Past The Post"
  p_ $ do
    "This voting system is the most simple, but often most flawed. "
    "Everyone gets one vote, the option with the most votes wins "
    
  p_ $ do
    "The issue with this system is that the least popular option can "
    "often win. For example, say that we are electing a president and "
    "there are two left wing candidates and one right wing candidate "
    "40% of the VAP may vote for the right wing candidate, and 30% "
    "vote for each left wing candidate. Then we have a right wing "
    "president even though the majority wanted a left wing one. "
    
  p_ "But it is, at least, simple"

  h3_ "Two Round first past the post"
  p_ $ do 
     "This is the same as first past the post, but done twice, the second time "
     "with the top scorers of the last round. You can control how many "
     "'winners' there are from the round before, but normally this is set to 2. "
     "The first vote happens, then another vote is started after, where the top "
     "two scorers are played off against each other"
  
  h3_ "Alternative Vote"
  p_ $ do
    "This is a much fairer form of election than the previous two when electing "
    "single outcomes. Voters rank how much they like each candidate/option. You "
    "You would put a '1' next to your favourate, '2' next to your second favourate "
    "and so on. You can rank as many and as few as you like"
    
  p_ $ do
    "The system works in rounds. In round 1, we count all of the first option votes "
    "If any candidate/option get's 50% of the available votes, they win. If not, "
    "we look at the person/option that got the least votes. We remove that option "
    "from the election, along with the votes. Anyone who marked that option as their "
    "first preference, has their second preference and we repeat."
    
  p_ $ do
    "For example: Say that we are electing a president. There are three candidates, "
    "one right wing (Alice) and two left wing (Bob and Charlie). 40% mark the right "
    "40% mark Alice as their first preference, 35% mark Bob as their first preference "
    "and 25% mark Charlie as their first preference. We will assume that the people "
    "who marked one left wing candidate as their first preference, marked the other as "
    "their second"
    
  p_ $ do
    "In round 1 the results are: Alice 40%, Bob: 35% and Charlie: 25%. "
    "No one gets over 50% so we move to round two! Charlie has lost and so is removed "
    "and anyone who marked Charlie as their first preference has their second preferenced "
    "added, which in these politically polaraising times means that Bob get's their votes. "
    "Now the results are: Alice 40%, Bob: 60%, so Bob wins!"
    
  p_ "This system is 'fairer' because vote splitting can't happen"

  h3_ "Supplementary Vote"
  p_ $ do
    "The voter is asked two questions:"
    ol_ $ do
      i_ "Who " 
      "or "
      i_ "which outcome "
      "do you want to win"
    ol_ "Who would you vote for in a runoff if your candidate didn't get through"
    "In the first round, if anyone gets over 50%, they win. If not, a run off happens "
    "with the top two options. Anyone who initially voted for them has their vote entered "
    "again, anyone who lost, but voted for one of them as their runoff candidate will have "
    "their runoff vote added."
  p_ $ do
    "It's a strange system, but it has it's benifits. It forces a more positive style "
    "of campaigning as candidates desire the second preferences of third parties."
  br_ []
  select_ [name_ "singSys", id_ "singSys"] $ do
    option_ [value_ "fptp"]    "First Past the Post"
    option_ [value_ "trftpt"]  "Two Round First Past the Post"
    option_ [value_ "altVote"] "Alternative Vote"
    option_ [value_ "supVote"] "Suplimentary Vote"



multiSys_ :: Monad m => HtmlT m ()
multiSys_ = div_ [id_ "multiSys", hidden_ "true"] $ do
  h2_ "Election System (Multiple Winners)"
  h3_ "First Past The Post"
  p_ $ do
    "This voting system is the most simple, but often most flawed. "
    "Everyone gets one vote, the option with the most votes wins. "
    "This only works where your voters are split up into groups, "
    "and each group elects a representative"
    
  p_ $ do
    "The issue with this system is that the least popular option can "
    "often win. For example, say that we are electing a president and "
    "there are two left wing candidates and one right wing candidate "
    "40% of the VAP may vote for the right wing candidate, and 30% "
    "vote for each left wing candidate. Then we have a right wing "
    "president even though the majority wanted a left wing one. "
    
  p_ "But it is, at least, simple"

  h3_ "Single Transferable Vote"
  p_ $ do
    "This is a much fairer system of voting than first past the post. "
    "In this system, voters rank candidates, where 1 is their favourate. "
    "You do not need to rank all of the candidates."
  p_ $ do
    "The way the counting works is, say we want to elect a group of 3 that is "
    "As representative as possible, then to "
    em_ "get elected, "
    "you need 33% of the vote share. We look at everyone's first preference "
    "And anyone who gets over 33% is elected. If not all of the seats are filled "
    "we move on to the next round. We remove the candidate who came last and we "
    "look at the second preference of the voters who marked the least popular "
    "candidate as their first preference. We repeate until all the seats are filled"
  p_ $ do
    "But we do more than just this! In this system, if 40% of people marked a winning "
    "candidate as their favourate, then 7% of them have essentally no voice! So as we "
    "move to the next rounds, we take their second preference as well, but we "
    em_ "weaken it. "
  p_ $ do
    "There are a few ways to do this, and you can select the method (and see the pros "
    "and cons) later."

  h3_ "Additional Member System"
  p_ $ do
    "This system requires voters to be split into groups, and candidates to be split into "
    "'parties'. In this system, the voter votes for a representative and party. A quota"
    "of seats are selected (by first past the post), say 80%. The remaining seats looks "
    "at how "
    em_ "unfair "
    "the election was. So say one party only got 30% of the seats by the first vote, "
    "but 40% of the party vote went to them, then the remaining seats are filled by the "
    "parties to make the election more propotional."

  h3_ "Two Round first past the post"
  p_ $ do 
     "This is the same as first past the post, but done twice, the second time "
     "with the top scorers of the last round. You can control how many "
     "'winners' there are from the round before, but normally this is set to 2. "
     "The first vote happens, then another vote is started after, where the top "
     "two scorers are played off against each other"

  h3_ "Alternative Vote"
  p_ $ do
    "Voters must be split into groups. Voters rank how much they like each candidate/option. You "
    "You would put a '1' next to your favourate, '2' next to your second favourate "
    "and so on. You can rank as many and as few as you like"
    
  p_ $ do
    "The system works in rounds. In round 1, we count all of the first option votes "
    "If any candidate/option get's 50% of the available votes, they win. If not, "
    "we look at the person/option that got the least votes. We remove that option "
    "from the election, along with the votes. Anyone who marked that option as their "
    "first preference, has their second preference and we repeat."
    
  p_ $ do
    "For example: Say that we are electing a president. There are three candidates, "
    "one right wing (Alice) and two left wing (Bob and Charlie). 40% mark the right "
    "40% mark Alice as their first preference, 35% mark Bob as their first preference "
    "and 25% mark Charlie as their first preference. We will assume that the people "
    "who marked one left wing candidate as their first preference, marked the other as "
    "their second"
    
  p_ $ do
    "In round 1 the results are: Alice 40%, Bob: 35% and Charlie: 25%. "
    "No one gets over 50% so we move to round two! Charlie has lost and so is removed "
    "and anyone who marked Charlie as their first preference has their second preferenced "
    "added, which in these politically polaraising times means that Bob get's their votes. "
    "Now the results are: Alice 40%, Bob: 60%, so Bob wins!"
    
  p_ "This system is 'fairer' because vote splitting can't happen"

  h3_ "Alternative Vote Plus"
  p_ $ do
    "This system is the same as the Additional Member System, but uses the Alternative "
    "Vote system instead of first past the post to elect the initial candidates. "

  h3_ "Supplementary Vote"
  p_ $ do
    "Voters are split into groups and asked two questions:"
    ol_ $ do
      i_ "Who " 
      "or "
      i_ "which outcome "
      "do you want to win"
    ol_ "Who would you vote for in a runoff if your candidate didn't get through"
    "In the first round, if anyone gets over 50%, they win. If not, a run off happens "
    "with the top two options. Anyone who initially voted for them has their vote entered "
    "again, anyone who lost, but voted for one of them as their runoff candidate will have "
    "their runoff vote added."
  p_ $ do
    "It's a strange system, but it has it's benifits. It forces a more positive style "
    "of campaigning as candidates desire the second preferences of third parties."

  h3_ "Party List Proportional Representation"
  p_ $ do
    "You vote for a party, as opposed to a person. The places are filled "
    "propotionally to the party votes."
  select_ [name_ "multiSys", id_ "multiSys"] $ do
    option_ [value_ "fptp"]     "First Past the Post"
    option_ [value_ "stv"]      "Single Transferable Vote"
    option_ [value_ "ams"]      "Additional Member System"
    option_ [value_ "trftpt"]   "Two Round First Past the Post"
    option_ [value_ "altVote"]  "Alternative Vote"
    option_ [value_ "altVoteP"] "Alternative Vote Plus"
    option_ [value_ "supVote"]  "Suplimentary Vote"
    option_ [value_ "plpr"]     "Party List Propotional Representation"


fptpSing_ :: Monad m => HtmlT m ()
fptpSing_ = div_ [id_ "fptpSing", hidden_ "true"] $ do
  h2_ "First Past The Post Settings"
  h3_ "Candidates/Options:"
  div_ [id_ "candidates"] ""
  button_ [type_ "button", id_ "fptpSingRemBtn"] "Remove"
  button_ [type_ "button", id_ "fptpSingAddBtn"] "Add"

pageData :: User -> CreateP
pageData u = CreateP
  { title = "Create Election"
  , errMsg = None
  , user  = u
  }
