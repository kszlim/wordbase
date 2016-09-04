module Main where

import Lib
import Data.Trie
import qualified Data.ByteString.Char8 as Char8

main :: IO ()
main = do
  dictionary <- dictionaryIO
  (board, neighbours) <- boardAndNeighboursIO
  let allCoordinates = [(x, y)| x <- [0..9], y <- [0..12]]
  let allWords = map (\coord -> findWordsAtCoordinate coord 5 board neighbours dictionary) allCoordinates
  print allWords
  return ()

dictionaryFilepath = "dist/resource/wordlist.txt"
wordlistFilepath = "dist/resource/board.txt"

dictionaryIO :: IO (Trie Char8.ByteString)
dictionaryIO = generateDictionary dictionaryFilepath

boardAndNeighboursIO :: IO (Board, Neighbours)
boardAndNeighboursIO = generateBoardAndNeighbours wordlistFilepath
