module Lib
    (
    generateDictionary,
    generateBoardAndNeighbours,
    findWordsAtCoordinate,
    findWordsAtCoordinate2,
    findAllWords,
    Neighbours,
    Board,
    Coordinate,
    ) where

import qualified Data.ByteString.Char8 as Char8
import Data.Trie
import Data.Char (toLower)
import Data.Vector ((!), Vector, generate, toList)
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe
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
  let strippedContent = if Char8.last contents == '\n' then Char8.init contents else contents
  let board = fromJust $ stringToBoard strippedContent
  let neighbours = getNeighbours $ Data.Vector.toList board
  return (board, neighbours)

stringToBoard :: Char8.ByteString -> Maybe (Vector BoardNode)
stringToBoard boardString = if length board /= 130 then Nothing else Just board
  where
    board = generate boardSize $ coordinates &&& Char8.index lowercaseBoard
    lowercaseBoard = Char8.pack $ map Data.Char.toLower (Char8.unpack boardString)
    coordinates index = (index `mod` horizontalBoardLength, quot index horizontalBoardLength)

getNeighbours :: [BoardNode] -> M.Map Coordinate [Coordinate]
getNeighbours board = M.fromList coordinatesList
  where
    coordinatesList = fmap (\(coordinate, node) -> (coordinate, allNeighbours coordinate)) board

getBoardNode :: Coordinate -> Vector BoardNode -> BoardNode
getBoardNode (x, y) board = board ! (x + y * horizontalBoardLength)

allNeighbours :: Coordinate -> [Coordinate]
allNeighbours (x, y) = filter (/= (x, y)) $ cartesianProduct (possibleXs x) (possibleYs y)
  where
    cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]
    possibleXs x = filter (\x -> x >= 0 && x <= maxXIndex) [x - 1, x, x + 1]
    possibleYs y = filter (\y -> y >= 0 && y <= maxYIndex) [y - 1, y, y + 1]

-- ALTERNATIVE
findWordsAtCoordinate2 :: Coordinate -> Integer -> Board -> Neighbours -> Trie Char8.ByteString -> [Char8.ByteString]
findWordsAtCoordinate2 (x, y) maxDepth board neighbours dictionary = findWords (Char8.singleton character) (x, y) [] rootDict (Set.singleton (x, y)) 0
  where
    (_, character) = getBoardNode (x, y) board
    rootDict = submap (Char8.singleton character) dictionary
    findWords prefix coordinate acc dict visited depth = foldr go acc (prefixesAndCoordinates prefix visited coordinate)
      where
        neighboursToVisit visited coordinate = map (`getBoardNode` board) $ filter (\coord -> not $ Set.member coord visited) (neighbours M.! coordinate)
        prefixesAndCoordinates prefix visited coordinate = map (\(coord, char) -> (Char8.snoc prefix char, coord)) (neighboursToVisit visited coordinate)
        tmpVisited = Set.insert coordinate visited
        go (tmpPrefix, coord) accumulator
          | depth > maxDepth || Data.Trie.null dict = accumulator
          | Data.Trie.member tmpPrefix dict = findWords tmpPrefix coord (tmpPrefix:accumulator) (submap tmpPrefix dict) tmpVisited (depth + 1)
          | otherwise = findWords tmpPrefix coord accumulator (submap tmpPrefix dict) tmpVisited (depth + 1)

findWordsAtCoordinate :: Coordinate -> Integer -> Board -> Neighbours -> Trie Char8.ByteString -> [Char8.ByteString]
findWordsAtCoordinate coordinate maxDepth board neighbours = helper basePrefix (Set.singleton coordinate) coordinate 0
  where
    basePrefix = Char8.singleton $ snd $ getBoardNode coordinate board
    helper prefix visited coordinate depth dict = foldr go [] (prefixesCoordinatesAndVisited prefix visited coordinate depth dict)
    prefixesCoordinatesAndVisited prefix visited coordinate depth dict = map context (neighboursToVisit visited coordinate)
      where
        context (coord, char) = (nextPrefix, coord, visited, depth, submap nextPrefix dict)
          where nextPrefix = Char8.snoc prefix char
    neighboursToVisit visited coordinate = map (`getBoardNode` board) $ filter (\coord -> not $ Set.member coord visited) (neighbours M.! coordinate)
    go (nextPrefix, coordinate, visited, depth, dict) accumulator
        | depth > maxDepth || Data.Trie.null dict = accumulator
        | Data.Trie.member nextPrefix dict = nextPrefix:accumulator ++ helper nextPrefix (Set.insert coordinate visited) coordinate depth dict
        | otherwise = accumulator ++ helper nextPrefix (Set.insert coordinate visited) coordinate depth dict

findAllWords :: Integer -> Board -> Neighbours -> Trie Char8.ByteString -> M.Map Coordinate [Char8.ByteString]
findAllWords maxDepth board neighbours dictionary = M.fromList $ map (\coord -> (coord, sortOn Char8.length (nub (findWordsAtCoordinate coord maxDepth board neighbours dictionary)))) allCoordinates
  where
    cartesian xs ys = [(x, y) | x <- xs, y <- ys]
    allCoordinates = cartesian [0..maxXIndex] [0..maxYIndex]
