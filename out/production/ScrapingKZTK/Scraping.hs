{-# LANGUAGE OverloadedStrings #-}

module Scraping where

import Text.HTML.Scalpel hiding (URL)
import Control.Applicative
import Control.Monad
import Text.Printf
import Data.Maybe
import Data.List (intercalate, nub)

type ATeam = [Player]
type BTeam = [Player]
data WinLose = Win | Lose deriving (Eq, Show)
data Player = Player {getChamp :: Champ, getUser :: User} deriving (Eq)

newtype URL = URL String deriving (Show)

data Item =  Item ATeam BTeam WinLose
newtype User = User String deriving (Eq, Show)
newtype Champ = Champ String deriving (Eq)

data Winner = WinA | WinB deriving (Eq)
data Matching = Matching ATeam BTeam Winner deriving (Eq)

instance Show Player where
  show (Player (Champ champ) user) = champ

instance Show Winner where
  show WinA = "A"
  show WinB = "B"

instance Show Matching where
  show (Matching a b winner) = intercalate "," $ show winner : map show a ++ map show b

-- クラス属性に空白入るとおかしい？
-- 空白に入る前までの文字列を入力すればOK
findUsers :: Scraper String [(User, URL)]
findUsers = chroots ("td" @: [hasClass "SummonerName"]) findUser

findUser :: Scraper String (User, URL)
findUser = do
  user <- text $ "a" @: [hasClass "Link"]
  url <- attr "href" $ "a" @: [hasClass "Link"]
  return (User user, URL url)

findItems :: Scraper String [Item]
findItems = chroots ("div" @: [hasClass "GameItemWrap"]) findItem

findItem :: Scraper String Item
findItem = do
  wl <- text $ "div" @: [hasClass "GameResult"]
  ab <- chroots ("div" @: [hasClass "Summoner"]) findAB
  let (aTeam, bTeam) = splitAt 5 ab :: ([Player], [Player])
      simpleWl = filter (/= '\n') . filter (/='\t') $ wl
      winLose = if simpleWl == "Victory" then Win else Lose
  return $ Item aTeam bTeam winLose

findAB :: Scraper String Player
findAB = do
  champ <- text $ "div" @: [hasClass "ChampionImage"] // "div"
  user <- text $ "div" @: [hasClass "SummonerName"] // "a"
  return $ Player (Champ champ) (User user)

judgeWinLoseList :: User -> [Item] -> [Matching]
judgeWinLoseList user = map judgeWinLose
  where
    judgeWinLose :: Item -> Matching
    judgeWinLose (Item a b wl)
      | wl == Win && user `elem` getUsers a = Matching a b WinA
      | wl == Win && user `elem` getUsers b = Matching a b WinB
      | wl == Lose && user `elem` getUsers a = Matching a b WinB
      | otherwise = Matching a b WinA

getUsers :: [Player] -> [User]
getUsers = map getUser

getAllMatchingList :: IO String
getAllMatchingList = do
  let overViewURL = "http://jp.op.gg/ranking/ladder/"
  Just usersAndURLs <- scrapeURL overViewURL findUsers
  matchingList <- forM usersAndURLs $ \(user, URL url) -> do
      items <- scrapeURL ("http:" ++ url) findItems
      return $ judgeWinLoseList user (fromMaybe [] items)

  return $ (unlines . map show . nub . concat) matchingList