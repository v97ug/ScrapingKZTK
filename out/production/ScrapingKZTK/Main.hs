

module Main where

import Scraping
--import Text.HTML.Scalpel hiding (URL)

--getAllMatchingList :: IO String
--getAllMatchingList = do
--  let overViewURL = "http://jp.op.gg/ranking/ladder/"
--  Just usersAndURLs <- scrapeURL overViewURL findUsers
--  matchingList <- forM usersAndURLs $ \(user, URL url) -> do
--      items <- scrapeURL ("http:" ++ url) findItems
--      return $ judgeWinLoseList user (fromMaybe [] items)
--
--  return $ (unlines . map show . nub . concat) matchingList

main :: IO ()
main = getAllMatchingList
  >>= writeFile "matching.csv"
--  >>= putStrLn

