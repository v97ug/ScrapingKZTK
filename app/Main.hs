

module Main where

import Scraping

main :: IO ()
main = getAllMatchingList
  >>= writeFile "matching.csv"
--  >>= putStrLn

