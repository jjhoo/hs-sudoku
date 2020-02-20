--
-- Copyright (c) 2013-2020 Jani J. Hakala <jjhakala@gmail.com> Finland
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as
--  published by the Free Software Foundation, version 3 of the
--  License.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
-- http://en.wikipedia.org/wiki/Sudoku
-- 9x9 grid with numbers 1-9, 3x3 sub-grids. Each number may occur once
-- in a row, column and sub-grid

{-

solve http://www.dailysudoku.com/sudoku/play.shtml?year=2013&month=09&day=19
which has solution
+-------+-------+-------+
| 8 1 4 | 6 7 2 | 3 9 5 |
| 2 5 6 | 9 3 1 | 4 8 7 |
| 7 9 3 | 8 4 5 | 1 2 6 |
+-------+-------+-------+
| 9 3 2 | 4 6 7 | 8 5 1 |
| 6 8 1 | 2 5 3 | 7 4 9 |
| 5 4 7 | 1 8 9 | 2 6 3 |
+-------+-------+-------+
| 4 7 8 | 5 1 6 | 9 3 2 |
| 3 2 5 | 7 9 4 | 6 1 8 |
| 1 6 9 | 3 2 8 | 5 7 4 |
+-------+-------+-------+
-}

{-
  General strategy: elimination of possible solutions by logic until
            a cell has only one possible solution

  Strategies for elimination / solving:
  1) single possible in a row / column / sub-grid
    -> solved cell
  2) pairs, triplets, quads
  3) hidden pairs, triplets, quads
  4) a candidate only in two cells in the same column or row, and in the
     same box
     -> eliminate others from the same box
-}

module Sudoku.Solver (
       Sudoku (..),
       convert,
       solve,
       newSolver,
       prettyPrint,
       prettyPrintCandidates) where

import Data.Char
import qualified Data.Choose   as Choose
import qualified Data.Permute  as Permute
import Data.Maybe

import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Arrow (second)
import Debug.Trace

{-# ANN module "HLint: warn Eta reduce" #-}
{-# ANN module "HLint: warn Use camelCase" #-}
{-# ANN module "HLint: warn Use curry" #-}
{-# ANN module "HLint: ignore Use infix" #-}
{-# ANN module "HLint: ignore Use section" #-}

-- a cell is either solved or known value
data CellValue a = Unsolved | Value a
     deriving (Eq, Show)

type Box = (Int, Int)
data Pos = Pos { row :: Int,
                 column :: Int,
                 box :: Box }
     deriving (Eq, Ord)

instance Show Pos where
     show pos@(Pos row col box) = "[" ++ show row ++ ", " ++ show col ++ "]"

boxNumber :: Int -> Int
boxNumber x = (+ 1) (quot (x - 1) 3)

newBox :: Int -> Int -> Box
newBox row col = (boxNumber row, boxNumber col)

newPos :: Int -> Int -> Pos
newPos row col = Pos row col (newBox row col)

------------------------------------------------------------------------
-- Test functions for cellcoordinates or pairs
inRow :: Pos -> Int -> Bool
inRow (Pos x1 x2 _) i = x1 == i

inCol :: Pos -> Int -> Bool
inCol (Pos x1 x2 _) i = x2 == i

inBox :: Pos -> Pos -> Bool
inBox = inSameBox

inSameRow :: Pos -> Pos -> Bool
inSameRow (Pos r1 _ _) (Pos r2 _ _) = r1 == r2

inSameCol :: Pos -> Pos -> Bool
inSameCol (Pos _ c1 _) (Pos _ c2 _) = c1 == c2

inSameBox :: Pos -> Pos -> Bool
inSameBox (Pos _ _ b1) (Pos _ _ b2) = b1 == b2

inSameLine :: Pos -> Pos -> Bool
inSameLine a b = inSameRow a b || inSameCol a b

sees :: Pos -> Pos -> Bool
sees a b = inSameRow a b || inSameCol a b || inSameBox a b

(<|>) = inSameCol
(<->) = inSameRow
(<#>) = inSameBox
(<*>) = sees

data Cell = Cell { pos :: Pos,
                   value :: CellValue Int }
     deriving (Show, Eq)

-- This grid holds the solved numbers 1,2,..,9; Unsolved if not yet solved
type SudokuGrid = Array (Int, Int) Cell

-- Keep the possible solutions in one list, otherwise would need
-- a list for each row, column and box that can be obtained by filtering
-- this list

-- Need to keep track of solved numbers in grid
-- and solution possibilities in candidates
data Sudoku = Sudoku { grid       :: SudokuGrid,
                       candidates :: [Cell],
                       loki       :: [String] }

data FindResult = FindResult { solved :: [Cell],
                               eliminated :: [Cell] }

-- create a new Cell
newCell :: Int -> Int -> Int -> Cell
newCell row col value
       | value == 0 = Cell (newPos row col) Unsolved
       | otherwise = Cell (newPos row col) (Value value)

-- get Value from Cell
cellNum :: Cell -> Int
cellNum (Cell _ Unsolved) = 0
cellNum (Cell _ (Value x)) = x

cellPos :: Cell -> Pos
cellPos (Cell pos _) = pos

-- Convert a string into a grid.
convert :: String -> SudokuGrid
convert str = A.listArray ((1, 1), (9, 9))
              [newCell (row i) (col i) (read [xxx !! i] :: Int) | i <- [0..80]]
  where xxx = filter isDigit str
        row i = (+ 1) (quot i 9)
        col i = (+ 1) (rem i 9)

newSolver :: SudokuGrid -> Sudoku
newSolver grid = Sudoku { grid = grid,
                         candidates = findCandidates grid,
                         loki = [] }

updateGrid :: SudokuGrid -> [Cell] -> SudokuGrid
updateGrid grid solved = grid A.// [((row, col), c) | c@(Cell pos@(Pos row col _) _) <- solved]

updateSolved :: [Cell] -> [Cell] -> [Cell]
updateSolved solved candidates = ncandidates
  where
    reject (Cell p1 v1) = filter (\(Cell p2 v2) -> not ((p1 == p2) || (p1 `sees` p2 && v1 == v2)))
    ncandidates = foldr reject candidates solved

updateCandidates :: [Cell] -> [Cell] -> [Cell]
updateCandidates solved candidates = ncandidates
  where
    reject (Cell p1 v1) = filter (\(Cell p2 v2) -> not (p1 == p2 && v1 == v2))
    ncandidates = foldr reject candidates solved

-- Convert a grid to printable string.
prettyPrint :: SudokuGrid -> String
prettyPrint bg = unlines [ startRow row ++
                           concat [printCell col (bg ! (row,col)) ++
                                   endCell row col
                                  | col <- [y0..y1]]
                            ++ endRow row
                         | row <- [x0..x1]]
  where
    ((x0, y0), (x1, y1)) = A.bounds bg
    endCell   x y | y < 9 && y `rem` 3 == 0     = " | "
                  | otherwise = " "

    printCell y (Cell _ Unsolved) = "."
    printCell y (Cell _ (Value num)) = show num

    startRow row | row == 1  = "+-------+-------+-------+\n| "
                 | otherwise = "| "
    endRow row | row `rem` 3 == 0 = "|\n+-------+-------+-------+"
               | otherwise   = "|"

-- Should do this box by box?
prettyPrintCandidates :: [Cell] -> String
prettyPrintCandidates xs = intercalate "\n" $
                           map (\cell@(Cell (Pos row col _) v) ->
                                 intercalate ", " (map show [row, col, cellNum cell])) xs


-- find candidates for each unsolved cell
findCandidates :: SudokuGrid -> [Cell]
findCandidates grid = trace ("findCandidates: " ++ show (length ncandidates)) ncandidates
  where
    psolved (pos, cell@(Cell _ Unsolved)) = False
    psolved (pos, cell@(Cell _ _)) = True

    solved = map snd $ filter psolved (A.assocs grid)
    candidates = [newCell x y n | x <- [1..9], y <- [1..9], cellNum (grid ! (x, y)) == 0, n <- [1..9]]

    reject (Cell p1 v1) = filter (\(Cell p2 v2) -> not (p1 `sees` p2 && v1 == v2))
    ncandidates = foldr reject candidates solved

(<=>) :: Cell -> Cell -> Bool
(<=>) (Cell _ v1) (Cell _ v2) = v1 == v2

boxSets :: [Cell] -> [[Cell]]
boxSets candidates = boxes
  where
    isBox box cell@(Cell (Pos r c b) _) = box == b

    boxes = filter (not . null) [ filter (isBox (r, c)) candidates | r <- [1..3], c <- [1..3] ]

colSets :: [Cell] -> [[Cell]]
colSets candidates = cols
  where
    isCol col cell@(Cell (Pos r c b) _) = col == c
    cols = filter (not . null) [ filter (isCol c) candidates | c <- [1..9] ]

rowSets :: [Cell] -> [[Cell]]
rowSets candidates = rows
  where
    isRow row cell@(Cell (Pos r c b) _) = row == r
    rows = filter (not . null) [ filter (isRow r) candidates | r <- [1..9] ]

lineSets :: [Cell] -> [[Cell]]
lineSets candidates = rowSets candidates ++ colSets candidates

allSets :: [Cell] -> [[Cell]]
allSets candidates = lineSets candidates ++ boxSets candidates

elemCombinations combf n elems = combf (Choose.elems chooser) :
      unfoldr (\chooser -> case Choose.next chooser of
              Just nchooser -> Just (combf (Choose.elems nchooser), nchooser)
              Nothing -> Nothing)
              chooser
  where
    nlen = length elems
    chooser = Choose.listChoose nlen n [0 .. (nlen - 1)]
    -- combf = Set.fromList . map (numbers !!)

elemPermutations permf elems = permf (Permute.elems permuter) :
      unfoldr (\permuter -> case Permute.next permuter of
              Just npermuter -> Just (permf (Permute.elems npermuter), npermuter)
              Nothing -> Nothing)
              permuter
  where
    nlen = length elems
    permuter = Permute.listPermute nlen [0 .. (nlen - 1)]

------------------------------------------------------------------------
-- Finders

-- For each row/column/box, find out the cells that have only solution left
findNakedSingles :: [Cell] -> FindResult
findNakedSingles candidates = FindResult found []
  where
    grouped = Map.fromListWith (++) [(pos, [cell]) | cell@(Cell pos value) <- candidates]
    singles = Map.filter ((== 1) . length) grouped
    found = map head $ Map.elems singles

-- For each row/column/box, find out if there is a number that is only in one cell
findHiddenSingles :: [Cell] -> FindResult
findHiddenSingles candidates = FindResult found []
  where
    singles = Map.filter ((== 1) . length)
    heads = map head . Map.elems

    groupByKey key xs = [((key pos, value), [cell]) | cell@(Cell pos@(Pos r c b) (Value value)) <- xs]
    foundx key xs = (heads . singles) $ Map.fromListWith (++) (groupByKey key xs)

    found1 = foundx (\(Pos r c b) -> r) candidates
    found2 = foundx (\(Pos r c b) -> c) candidates
    found3 = foundx (\(Pos r c b) -> b) candidates

    found = nub (found1 ++ found2 ++ found3)

findSets :: Int -> (Set Int -> Set Int -> Bool) -> (Set Pos -> Set Int -> Cell -> Bool) -> [Cell] -> FindResult
findSets n pmatch pelim candidates = FindResult [] found
  where
    groupValueByPos = map (\(Cell pos (Value value)) -> (pos, [value]))

    cellNumbers = Map.fromListWith (++) . groupValueByPos
    cellNumberSets = Map.map Set.fromList . cellNumbers

    setNumbers = nub . map cellNum

    find :: [Cell] -> [Cell]
    -- find set | trace ("find set " ++ show n ++ " " ++ show set) False = undefined
    find set | length numbers >= n = xfound
             | otherwise = []
      where
        cellSets = cellNumberSets set
        numbers = setNumbers set
        xfound = loop (combs numbers)

        combs = elemCombinations (Set.fromList . map (numbers !!)) n

        -- remove candidates that have matching numbers
        h :: Set Int -> Map Pos (Set Int) -> [Cell]
        h cset xs = if Map.size xs == n then
                       filter (pelim poss cset) set
                    else
                       []

          where
            poss = Map.keysSet xs

        g :: Set Int -> Map Pos (Set Int) -> Map Pos (Set Int)
        g cset = Map.filter (pmatch cset)

        f cset = h cset (g cset cellSets)

        loop = concatMap f

    found = (nub . concatMap find) (allSets candidates)

-- for all sets,
--   get numbers and (pos, set of numbers)
--   skip set if number of positions equals n
--   generate combinations for numbers
--   check if there are exactly n cells that contain only n numbers of a set
--   eliminate those numbers from other cells
findNakedSets :: Int -> [Cell] -> FindResult
findNakedSets n candidates = findSets n pmatch pelim candidates
  where
    pelim poss cset cell@(Cell pos (Value value)) = not (Set.member pos poss) && Set.member value cset
    pmatch cset = flip Set.isSubsetOf cset

-- for all sets,
--   get numbers and (pos, set of numbers)
--   skip set if number of positions equals n
--   generate combinations for numbers
--   check if there are exactly n cells that contain numbers of the set
--   eliminate other numbers from those cells
findHiddenSets :: Int -> [Cell] -> FindResult
findHiddenSets n candidates = findSets n pmatch pelim candidates
  where
    pelim poss cset cell@(Cell pos (Value value)) = Set.member pos poss && not (Set.member value cset)
    pmatch cset nset = Set.size (Set.intersection nset cset) > 0

-- if there are two or three cells that,
--   have the same value n,
--   are on the same row or column, and
--   are the only candidates for n in the box,
-- then other candidates from the same row or column can be eliminated
findPointingPairs :: [Cell] -> FindResult
findPointingPairs candidates = FindResult [] found
  where
    groupPosByValue :: [Cell] -> [(Int, [Pos])]
    groupPosByValue = map (\(Cell pos (Value value)) -> (value, [pos]))

    numberPoss :: [Cell] -> Map Int [Pos]
    numberPoss = Map.fromListWith (++) . groupPosByValue

    setNumbers = nub . map cellNum

    -- for all boxes,
    --   get numbers and (number, set of positions)
    --   for all numbers n,
    --     next if number of positions is not between 2 and 3
    --     next if positions are not on the same row or column
    --

    find :: [Cell] -> [Cell]
    -- find set | trace ("find pointing pair in box " ++ show set) False = undefined
    find set = loop numbers
      where
        numbers = setNumbers set

        gr :: Int -> [Pos] -> [Cell]
        gr n set = others
          where
            (Pos row _ box) = head set
            others = filter (\cell@(Cell pos@(Pos r c b) (Value value)) ->
                              row == r && n == value && box /= b)
                            candidates

        gc :: Int -> [Pos] -> [Cell]
        gc n set = others
          where
            (Pos _ col box) = head set
            others = filter (\cell@(Cell pos@(Pos r c b) (Value value)) ->
                              col == c && n == value && box /= b)
                            candidates

        f :: Int -> [Cell] -> [Cell]
        f n boxset
          | isrset = gr n xs
          | iscset = gc n xs
          | otherwise = []
          where
            npset = numberPoss boxset
            xs = (Map.!) npset n

            isrset = allInSet (<->) xs
            iscset = allInSet (<|>) xs

            allInSet :: (Pos -> Pos -> Bool) -> [Pos] -> Bool
            allInSet p [] = False
            allInSet p [pos0] = False
            allInSet p (pos0:rest) = all (p pos0) rest

        loop = concatMap (flip f set)

    found = (nub . concatMap find) (boxSets candidates)

-- if there are two or three cells that,
--   have the same value n,
--   are on the same row or column,
--   are located in the same box, and
--   are the only candidates for n in the row or column,
--
-- then other candidates from the same box can be eliminated
findBoxlineReduction :: [Cell] -> FindResult
findBoxlineReduction candidates = FindResult [] found
  where
    groupPosByValue :: [Cell] -> [(Int, [Pos])]
    groupPosByValue = map (\(Cell pos (Value value)) -> (value, [pos]))

    numberPoss :: [Cell] -> Map Int [Pos]
    numberPoss = Map.fromListWith (++) . groupPosByValue

    setNumbers = nub . map cellNum

    find :: (Pos -> Pos -> Bool) -> [Cell] -> [Cell]
    -- find lineset | trace ("find box/line reduction in line " ++ show lineset) False = undefined
    find psameline lineset = loop numbers
      where
        numbers = setNumbers lineset

        g :: Int -> [Pos] -> [Cell]
        g n set = others
          where
            xpos@(Pos row col box) = head set
            others = filter (\cell@(Cell pos@(Pos r c b) (Value value)) ->
                              not (xpos `psameline` pos) && n == value && box == b)
                            candidates

        f :: Int -> [Cell] -> [Cell]
        f n set
          | isset = g n xs
          | otherwise = []
          where
            npset = numberPoss set
            xs = (Map.!) npset n

            isset = allInSet psameline xs && allInSet (<#>) xs

            allInSet :: (Pos -> Pos -> Bool) -> [Pos] -> Bool
            allInSet p [] = False
            allInSet p [pos0] = False
            allInSet p (pos0:rest) = all (p pos0) rest

        loop = concatMap (flip f lineset)


    found = nub (concatMap (find (<->)) (rowSets candidates) ++ concatMap (find (<|>)) (colSets candidates))

-- if there are four cells that
--   - contain same candidate value n
--   - form corners of a box, and
--   - two row (column) lines going through those cells
--     have candidates of n only on those cells
--
-- then other candidates on column (row) lines going through
-- those cells can be eliminated
--
findXWings :: [Cell] -> FindResult
findXWings candidates = FindResult [] (find inSameCol (rowSets candidates) ++ find inSameRow (colSets candidates))
  where
    find :: (Pos -> Pos -> Bool) -> [[Cell]] -> [Cell]
    -- find _ linesets | trace ("find xwings " ++ show lineset) False = undefined
    find inPerpendicular linesets
         | nsets < 2 = []
         | otherwise = concatMap linePairTest (lineCombinations linesets)
      where
        nsets = length linesets
        lineCombinations linesets = elemCombinations (map (linesets !!)) 2 linesets

        linePairTest [] = []
        linePairTest [line1, line2] = concatMap (\(n, pos1, pos4) ->
                                              filter (\cell@(Cell pos (Value num)) ->
                                                      n == num
                                                      && (pos1 `inPerpendicular` pos || pos4 `inPerpendicular` pos)
                                                      && notElem pos poss)
                                              candidates)
                                              wtf
          where
            poss = map cellPos (line1 ++ line2)
            nums1 = (Set.fromList . map cellNum) line1
            nums2 = (Set.fromList . map cellNum) line2
            common = Set.intersection nums1 nums2

            f :: Int -> [Cell] -> [Cell]
            f n = filter (cellMatch n)

            wtf = mapMaybe (\n -> maybeXwing n (f n line1) (f n line2)) (Set.elems common)

        cellMatch :: Int -> Cell -> Bool
        cellMatch n = (n ==) . cellNum

        --        cell1  cell2  or  cell1 cell3
        --        cell3  cell4      cell2 cell4
        maybeXwing n [] [] = Nothing
        maybeXwing n [cell1@(Cell pos1 _), cell2@(Cell pos2 _)] [cell3@(Cell pos3 _), cell4@(Cell pos4 _)]
                | pos1 `inPerpendicular` pos3 && pos2 `inPerpendicular` pos4 = Just (n, pos1, pos4)
                | otherwise = Nothing
        maybeXwing n _ _ = Nothing

-- if there are three cells wing1, hinge, wing2 that
--   - wing1 sees hinge, wing2 sees wegdge,
--   - hinge sees wing1 and wing2
--   - contain numbers XYZ so that XZ - XY - YZ
--   - then candidates Z seen by both wings can be eliminated
--
findYWings :: [Cell] -> FindResult
findYWings candidates
   | Map.size pairs < 3 = FindResult [] []
   | otherwise = FindResult [] found
  where
    groupValueByPos = map (\(Cell pos (Value value)) -> (pos, [value]))
    cellNumbers = Map.fromListWith (++) . groupValueByPos

    getPairs = Map.map Set.fromList . Map.filter ((==) 2 . length) . cellNumbers
    pairs = getPairs candidates
    pairsPos = Map.keys pairs

    hasThreeNumbers :: Map Pos (Set Int) -> [Pos] -> Bool
    hasThreeNumbers pairs comb = (==) 3 (Set.size (combNumbers comb))
      where
        combNumbers = foldr (\pos acc -> Set.union acc ((Map.!) pairs pos)) Set.empty

    isWing :: [Pos] -> Bool
    isWing [] = False
    isWing [wing1, hinge, wing2] = (wing1 `sees` hinge && hinge `sees` wing2)
                                     && not (wing1 <#> hinge && hinge <#> wing2)
                                     && not (wing1 <-> hinge && hinge <-> wing2)
                                     && not (wing1 <|> hinge && hinge <|> wing2)
    isWing _ = False

    ywings ::  Map Pos (Set Int) -> [Pos] -> [[Pos]]
    ywings pairs comb
           | hasThreeNumbers pairs comb = filter isWing (perms comb)
           | otherwise = []
           where
             perms :: [Pos] -> [[Pos]]
             perms = elemPermutations (map (comb !!))

    canEliminate :: Map Pos (Set Int) -> [Pos] -> [Cell]
    canEliminate pairs [wing1, hinge, wing2]
                 | not (Set.size nset == 1 && c2 `Set.isSubsetOf` xset) = []
                 | otherwise =  filter (\cell@(Cell pos (Value value)) ->
                                        value == n && pos `sees` wing1 && pos `sees` wing2
                                        && notElem pos [wing1, hinge, wing2])
                                        candidates
      where
        c1 = (Map.!) pairs wing1
        c2 = (Map.!) pairs hinge
        c3 = (Map.!) pairs wing2
        nset = Set.intersection c1 c3
        xset = Set.difference (Set.union c1 c3) nset
        n = (head . Set.toList) nset

    others ::  Map Pos (Set Int) -> [[Pos]] -> [Cell]
    others pairs [] = []
    -- others pairs wings | trace ("find ywings " ++ show wings) False = undefined
    others pairs wings = concatMap (canEliminate pairs) wings

    find pairs = concatMap (others pairs . nub . ywings pairs) (combs (Map.keys pairs))
      where
        combs poss = elemCombinations (map (poss !!)) 3 poss

    found = nub (find pairs)


-- if there are three cells wing1, hinge, wing2 that
--   - wing1 sees hinge, wing2 sees wegdge,
--   - hinge sees wing1 and wing2
--   - contain numbers XYZ so that XZ - XYZ - YZ
--   - then candidates Z seen by all three can be eliminated
--
findXYZWings :: [Cell] -> FindResult
findXYZWings candidates
   | Map.size triples < 3 = FindResult [] []
   | otherwise = FindResult [] found
  where
    psize n = n == 2 || n == 3

    groupValueByPos = map (\(Cell pos (Value value)) -> (pos, [value]))
    cellNumbers = Map.fromListWith (++) . groupValueByPos

    getTriples = Map.map Set.fromList . Map.filter (psize . length) . cellNumbers
    triples = getTriples candidates
    triplesPos = Map.keys triples

    hasThreeNumbers :: Map Pos (Set Int) -> [Pos] -> Bool
    hasThreeNumbers triples comb = psize (Set.size (combNumbers comb))
      where
        combNumbers = foldr (\pos acc -> Set.union acc ((Map.!) triples pos)) Set.empty

    isWing :: [Pos] -> Bool
    isWing [] = False
    isWing [wing1, hinge, wing2] = (wing1 `sees` hinge && hinge `sees` wing2)
                                     && not (wing1 <#> hinge && hinge <#> wing2)
                                     && not (wing1 <-> hinge && hinge <-> wing2)
                                     && not (wing1 <|> hinge && hinge <|> wing2)
    isWing _ = False

    ywings ::  Map Pos (Set Int) -> [Pos] -> [[Pos]]
    ywings triples comb
           | hasThreeNumbers triples comb = filter isWing (perms comb)
           | otherwise = []
           where
             perms :: [Pos] -> [[Pos]]
             perms = elemPermutations (map (comb !!))

    canEliminate :: Map Pos (Set Int) -> [Pos] -> [Cell]
    -- canEliminate triples wing | trace ("find xyzwings " ++ show wing) False = undefined
    canEliminate triples [wing1, hinge, wing2]
                 | not (Set.size c1 == 2 && Set.size c2 == 3 && Set.size c3 == 2) = []
                 | not (Set.size nset == 1 && c2 == wset) = []
                 | otherwise = filter (\cell@(Cell pos (Value value)) ->
                                       value == n
                                       && all (pos `sees`) [wing1, hinge, wing2]
                                       && notElem pos [wing1, hinge, wing2])
                                       candidates
      where
        c1 = (Map.!) triples wing1
        c2 = (Map.!) triples hinge
        c3 = (Map.!) triples wing2

        wset = Set.union c1 c3
        nset = Set.intersection (Set.intersection c1 c2) c3
        n = (head . Set.toList) nset

    others ::  Map Pos (Set Int) -> [[Pos]] -> [Cell]
    others triples [] = []
    -- others triples wings | trace ("find ywings " ++ show wings) False = undefined
    others triples wings = concatMap (canEliminate triples) wings

    find triples = concatMap (others triples . nub . ywings triples) (combs (Map.keys triples))
      where
        combs poss = elemCombinations (map (poss !!)) 3 poss

    found = nub (find triples)

strategies = [ ("naked singles", findNakedSingles),
               ("hidden singles", findHiddenSingles),
               ("naked pairs", findNakedSets 2),
               ("naked triples", findNakedSets 3),
               ("hidden pairs", findHiddenSets 2),
               ("hidden triples", findHiddenSets 3),
               ("naked quads", findNakedSets 4),
               ("hidden quads", findHiddenSets 4),
               ("pointing pairs", findPointingPairs),
               ("box/line reduction", findBoxlineReduction),
               ("x-wings", findXWings),
               ("y-wings", findYWings),
               ("xyz-wings", findXYZWings) ]

-- |Function 'solve' attempts to solve the given sudoku puzzle
solve :: Sudoku -> Sudoku
solve state@(Sudoku sgrid candidates _) = solvestep strategies state

-- step through strategies
solvestep :: [(String, [Cell] -> FindResult)] -> Sudoku -> Sudoku
solvestep [] state = state
solvestep ((fdesc, f):fs) state@(Sudoku sgrid candidates loki)
  | success     = trace ("found something to eliminate by " ++ show fdesc ++ ": " ++ show (candidates \\ ncandidates)) solve (Sudoku nsgrid ncandidates nloki)
  | otherwise   = solvestep fs state
  where
    result@(FindResult solved eliminated) = f candidates

    success = (solved, eliminated) /= ([], [])

    nsgrid = updateGrid sgrid solved
    update = (updateCandidates eliminated) . (updateSolved solved)
    ncandidates = update candidates
    -- nloki = addToLog fdesc (candidates \\ ncandidates) (gridDiff2 sgrid nsgrid) loki
    nloki = trace ("solvestep: " ++ show (length candidates) ++ " -> " ++ show (length ncandidates)) loki

    addToLog fdesc candidates grid loki =
      ("Solved by " ++  fdesc ++ ": " ++ show grid ++ "\n\teliminated: " ++ show candidates ++ "\n") : loki
