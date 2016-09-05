module Main where

import Lib
import Data.Trie
import qualified Data.ByteString.Char8 as Char8
import Data.Map ((!))

main :: IO ()
main = do
  dict <- dictionaryIO
  (board, neighbours) <- boardAndNeighboursIO
  let allWords = findAllWords 100 board neighbours dict
  print allWords
  return ()

dictionaryFilepath = "dist/resource/wordlist.txt"
wordlistFilepath = "dist/resource/test.txt"

dictionaryIO :: IO (Trie Char8.ByteString)
dictionaryIO = generateDictionary dictionaryFilepath

boardAndNeighboursIO :: IO (Board, Neighbours)
boardAndNeighboursIO = generateBoardAndNeighbours wordlistFilepath
