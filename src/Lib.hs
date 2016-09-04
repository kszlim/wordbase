module Lib
    (
    generateDictionary,
    generateBoardAndNeighbours,
    findWordsAtCoordinate,
    Neighbours,
    Board,
    Coordinate
    ) where

import qualified Data.ByteString.Char8 as Char8
import Data.Trie
import Data.Vector ((!), Vector, generate, toList)
import qualified Data.Map.Strict as M
import Control.Arrow ((&&&))
import qualified Data.Set as Set

horizontalBoardLength = 10
verticalBoardLength = 13
minXYIndex = 0
maxXIndex = horizontalBoardLength - 1
maxYIndex = verticalBoardLength - 1
boardSize = horizontalBoardLength * verticalBoardLength

type Coordinate = (Int, Int)
type Value = Char
type BoardNode = (Coordinate, Value)
type Board = Vector BoardNode
type Neighbours = M.Map Coordinate [Coordinate]

stringsToTrie :: [Char8.ByteString] -> Trie Char8.ByteString
stringsToTrie strings = Data.Trie.fromList $ zip strings strings

generateDictionary :: FilePath -> IO (Trie Char8.ByteString)
generateDictionary filepath = do
  contents <- Char8.readFile filepath
  let wordlist = Char8.lines contents
  return $ stringsToTrie wordlist

generateBoardAndNeighbours :: FilePath -> IO (Board, Neighbours)
generateBoardAndNeighbours filepath = do
  contents <- Char8.readFile filepath
  let board = stringToBoard contents
  let neighbours = getNeighbours $ Data.Vector.toList board
  return (board, neighbours)

stringToBoard :: Char8.ByteString -> Vector BoardNode
stringToBoard boardString = generate boardSize $ coordinates &&& Char8.index boardString
  where
    coordinates index = (index `mod` horizontalBoardLength, quot index horizontalBoardLength)

getNeighbours :: [BoardNode] -> M.Map Coordinate [Coordinate]
getNeighbours board = M.fromList coordinatesList
  where
    coordinatesList = fmap (\(coordinate, node) -> (coordinate, allNeighbours coordinate)) board

getBoardNode :: Coordinate -> Vector BoardNode -> BoardNode
getBoardNode (x, y) board = board ! (x + y * horizontalBoardLength)

allNeighbours :: Coordinate -> [Coordinate]
allNeighbours (x, y) = filter (== (x, y)) $ cartesianProduct (possibleXs x) (possibleYs y)
  where
    cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]
    possibleXs x = filter (\x -> x > 0 && x < maxXIndex) [x - 1, x, x + 1]
    possibleYs y = filter (\y -> y > 0 && y < maxYIndex) [y - 1, y, y + 1]

findWordsAtCoordinate :: Coordinate -> Integer -> Board -> Neighbours -> Trie Char8.ByteString -> [Char8.ByteString]
findWordsAtCoordinate (x, y) maxDepth board neighbours dictionary = findWords (Char8.singleton character) (x, y) [] rootDict (Set.singleton (x, y)) 0
  where
    ((rootX, rootY), character) = getBoardNode (x, y) board
    rootDict = submap (Char8.singleton character) dictionary
    findWords prefix coordinate acc dict visited depth =
        foldr go acc prefixesAndCoordinates
      where
        neighboursToVisit = map (`getBoardNode` board) $ filter (`Set.member` visited) (neighbours M.! coordinate)
        prefixesAndCoordinates = map (\(coord, char) -> (Char8.cons char prefix, coord)) neighboursToVisit
        tmpVisited = Set.insert coordinate visited
        go (tmpPrefix, coord) accumulator =
          if depth > maxDepth || Data.Trie.null dict then accumulator else
            findWords prefix coord accumulator (submap tmpPrefix dict) tmpVisited (depth + 1)
