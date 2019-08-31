--
-- Copyright (c) 2013-2019 Jani J. Hakala <jjhakala@gmail.com>
--                                        Jyväskylä, Finland
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

-- Mark unsolved cells as 0, solved as [1..9]
--  -- Changing to   data Cell a = Unsolved | Cell a
--     didn't change things much as most of the work involves the
--     the candidate list that has numbers [1..9]
--
-- Go through strategies, if a strategy reduces number of candidates,
-- restart starting from simple strategies
--
-- like in this sudoku solver, http://www.sudokuwiki.org/sudoku.htm
--
-- Notes / Todo:
-- - generalise getNakedPairs / getNakedTriples / getNakedQuads etc.
-- - better data structures for the task?
-- - extra work may be done because using [1..9] in many places,
--   solved numbers are not skipped
-- - check for proper partial / whole solutions, abort early if not
--
-- - needed to google about trace / traceShow
-- - started to do more function composition (.) while doing this
--
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

module Main (newSolver, solve, prettyPrint, main) where

import           Data.Array    (Array, (!))
import qualified Data.Array    as A
import           Data.Char
import           Data.List

import           Control.Arrow (second)
import           Debug.Trace

{-# ANN module "HLint: warn Eta reduce" #-}
{-# ANN module "HLint: warn Use camelCase" #-}
{-# ANN module "HLint: warn Use curry" #-}
{-# ANN module "HLint: ignore Use infix" #-}

-- a cell is either solved or known value
data Cell a = Unsolved | Cell a
            deriving (Show, Eq)

type CellCoordinate = (Int, Int)

-- This grid holds the solved numbers 1,2,..,9; Unsolved if not yet solved
type SudokuGrid = Array CellCoordinate (Cell Int)

-- Keep the possible solutions in one list, otherwise would need
-- a list for each row, column and box that can be obtained by filtering
-- this list
type Candidate  = (CellCoordinate, Int)

-- Need to keep track of solved numbers in grid
-- and solution possibilities in candidates
data Sudoku = Sudoku { grid       :: SudokuGrid,
                       candidates :: [Candidate],
                       loki       :: [String] }

-- convert from number to Cell
toCell :: Int -> Cell Int
toCell x | x == 0 = Unsolved
         | otherwise = Cell x

-- convert from Cell to number
cellNum :: Cell Int -> Int
cellNum Unsolved = 0
cellNum (Cell x) = x

-- Convert a string into a grid.
convert :: String -> SudokuGrid
convert str = A.listArray ((1, 1), (9, 9))
              [toCell (read [xxx !! i] :: Int)| i <- [0..80]]
  where xxx = filter isDigit str

newSolver :: SudokuGrid -> Sudoku
newSolver grid = Sudoku { grid = grid,
                         candidates = findCandidates grid,
                         loki = [] }

-- find candidates for each unsolved cell
findCandidates :: SudokuGrid -> [Candidate]
findCandidates grid = nxs
  where
    solved = map (second cellNum) $ filter (\(c, v) -> v /= Unsolved) (A.assocs grid)
    xs = [((x, y), n) | x <- [1..9], y <- [1..9], n <- [1..9],
          grid ! (x, y) == Unsolved]

    psolved x@(c1, vx) y@(c2, vy) = c1 == c2
    prowcol x@(c1, vx) y@(c2, vya) = c1 /= c2 && x <=> y
                                     && (c1 <-> c2 || c1 <|> c2)
    pbox x@(c1, vx) y@(c2, vy)    = c1 /= c2 && x <=> y && c1 <#> c2

    nxs = foldr (\p acc -> update p solved acc) xs [psolved,
                                                    prowcol,
                                                    pbox]
    update p singles xs = eraseAllBy p xs singles

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

    printCell y Unsolved   = "."
    printCell y (Cell num) = show num

    startRow row | row == 1  = "+-------+-------+-------+\n| "
                 | otherwise = "| "
    endRow row | row `rem` 3 == 0 = "|\n+-------+-------+-------+"
               | otherwise   = "|"

-- Should do this box by box?
prettyPrintCandidates :: [Candidate] -> String
prettyPrintCandidates xs = intercalate "\n" $
                           map (\((x, y), v) ->
                                 (intercalate "," (map show [x, y, v]))) xs

------------------------------------------------------------------------
-- 3x3 box helpers
boxes :: [(CellCoordinate, CellCoordinate)]
boxes = [((i, j), (i + 2, j + 2)) | i <- [1,4,7], j <- [1,4,7]]

boxBounds :: Int -> (CellCoordinate, CellCoordinate)
boxBounds i = boxes !! (i - 1)

-- return the bounds for the box where the cell indicated by x and y is
xyToBox :: Int -> Int -> ((Int,Int), CellCoordinate)
xyToBox x y = head (filter (\((x1, y1), (x2, y2)) ->
                             and [x1 <= x, x2 >= x,
                                  y1 <= y, y2 >= y]) boxes)


xyToBoxN x y = (+ 1) (((x - 1) `div` 3) * 3 + ((y - 1) `div` 3))

------------------------------------------------------------------------
-- Test functions for cellcoordinates or pairs
inRow :: CellCoordinate -> Int -> Bool
inRow a@(x1, x2) i = x1 == i

inCol :: CellCoordinate -> Int -> Bool
inCol a@(x1, x2) i = x2 == i

inBox :: CellCoordinate -> CellCoordinate -> Bool
inBox = inSameBox

inSameRow :: CellCoordinate -> CellCoordinate -> Bool
inSameRow a@(x1, x2) b@(y1, y2) = x1 == y1

inSameCol :: CellCoordinate -> CellCoordinate -> Bool
inSameCol a@(x1, x2) b@(y1, y2) = x2 == y2

inSameBox :: CellCoordinate -> CellCoordinate -> Bool
inSameBox a@(x1, x2) b@(y1, y2) = xyToBoxN x1 x2 == xyToBoxN y1 y2

haveSameValue :: Candidate -> Candidate -> Int -> Bool
haveSameValue (_, v1) (_, v2) n = v1 == v2 && v1 == n

(<|>) = inSameCol
(<->) = inSameRow
(<#>) = inSameBox

(<=>) :: Candidate -> Candidate -> Bool
(<=>) (_, v1) (_, v2) = v1 == v2
------------------------------------------------------------------------

-- return unique cell coordinates for a set of candidates
toCells :: [Candidate] -> [CellCoordinate]
toCells cands = nub . map fst $ cands

-- return possible solutions at cell (i, j)
getCellC :: CellCoordinate -> [Candidate] -> [Candidate]
getCellC (i, j) = filter ((==) (i, j) . fst)

-- return possible solutions at cell (i, j) as a list
getCellCA :: CellCoordinate -> [Candidate] -> (CellCoordinate, [Int])
getCellCA (i, j) = (,) (i, j) . map snd . getCellC (i, j)

-- convert from [Candidate] to [(CellCoordinate, [Int])] list
-- easier to handle in some cases
getCA :: [((Int,Int), [Int])] -> [Candidate] -> [(CellCoordinate, [Int])]
getCA prev [] = map (second reverse) prev
getCA [] ((cell,v):xs) = getCA [(cell, [v])] xs
getCA prev@((ycell, yvals):ys) ((xcell, xval):xs)
  | xcell == ycell  = getCA ((ycell, xval : yvals) : ys) xs
  | otherwise   = getCA ((xcell, [xval]) : prev) xs

-- return possible solutions for given row as array
getRowCA :: Int -> [Candidate] -> [(CellCoordinate, [Int])]
getRowCA i = getCA [] . getRowC i

-- return possible solutions for given column as array
getColCA :: Int -> [Candidate] -> [(CellCoordinate, [Int])]
getColCA i = getCA [] . getColC i

-- return possible solutions for given box as array
getBoxCA :: Int -> [Candidate] -> [(CellCoordinate, [Int])]
getBoxCA i = getCA [] . getBoxC i

-- return possible solutions for given row
getRowC :: Int -> [Candidate] -> [Candidate]
getRowC i = filter ((== i) . fst . fst)

getColC :: Int -> [Candidate] -> [Candidate]
getColC i = filter ((== i) . snd . fst)

getBoxC :: Int -> [Candidate] -> [Candidate]
getBoxC i = filter (f i)
  where
    f box ((r, c), v) = i == xyToBoxN r c

------------------------------------------------------------------------

-- 1) For each row/column/box, find out the cells that have only solution left
--    or if a row/column/box has only one cell where an unsolved number is.
-- 2) Remove solved numbers from grid
-- 3) Remove solved numbers from Candidates,
--    from row/column/box they affect
eliminateSingles :: Sudoku -> Sudoku
eliminateSingles state@(Sudoku sgrid xs loki) =
  -- trace ("eliminateSingles: " ++ show (xs \\ updateCandidates ngrid xs))
  Sudoku ngrid nxs loki
  where
    (ngrid, nxs) = foldr (\i -> step i getBoxC . step i getColC . step i getRowC) (sgrid, xs) [1..9]

    step x f acc@(grid, xs)
      | null singles = acc
      | otherwise = eliminate singles acc
      where
        singles = getSingles (f x xs)

    eliminate singles acc@(grid, xs) = (ngrid, nxs)
      where
        ngrid = grid A.// [(c, toCell v) | (c, v) <- singles]

        -- solved cells.
        psolved x@(c1, vx) y@(c2, vy) = c1 == c2
        -- eliminated by solved cells in lines
        prowcol x@(c1, vx) y@(c2, vy) = c1 /= c2 && x <=> y
                                        && (c1 <-> c2 || c1 <|> c2)
        -- eliminated by solved cells in box
        pbox x@(c1, vx) y@(c2, vy)    = c1 /= c2 && x <=> y && c1 <#> c2

        nxs = foldr (\p acc -> update p singles acc) xs [psolved,
                                                         prowcol,
                                                         pbox]
    -- is this almost like deleteFirstBy... no?
    -- update p singles xs = xs \\ [x | x <- xs, y <- singles, p x y]
    update p singles xs = eraseAllBy p xs singles

-- calls candidate elimination function f for each row/cell/box
eliminator f xs = nxs
  where
    nxs = foldr (\i -> stepB i . stepC i . stepR i) xs [1..9]

    stepR row acc0 = acc0 \\ eliminate acc0
      where
        p (r, c) = r == row
        eliminate = f "row" (getRowC row xs) getRowCA p row acc0

    stepC col acc0 = acc0 \\ eliminate acc0
      where
        p (r, c) = c == col
        eliminate = f "col" (getColC col xs) getColCA p col acc0

    stepB box acc0 = acc0 \\ eliminate acc0
      where
        p (r, c) = xyToBoxN r c == box
        eliminate = f "box" (getBoxC box xs) getBoxCA p box acc0

-- 1) For each row/column/box, find out the cells that share
--    a pair of numbers as their only possible solutions.
-- 2) Remove these numbers from Candidates list, for each
--    row/column/box the pairs affects
eliminatePairs :: Sudoku -> Sudoku
eliminatePairs state@(Sudoku sgrid xs _) = state {candidates = nxs}
  where
    nxs = eliminator stepX xs

    stepX desc cands fun2 p i xs
      -- need more than 2 cells in order to be of any use
      | cellCount > 2 && pairlen > 0	= stepX2 desc cands pairs fun2 p i xs
      | otherwise           = const []
      where
        -- number of unsolved cells in row/column/box
        cellCount = length $ toCells cands

        pairs = getNakedPairs cands
        pairlen = length pairs

    stepX2 desc cands pairs fun2 p i xs = eliminate
      where
        -- those cells that do not contain one of the pairs
        otherCells = map fst . filter (flip notElem pairs . snd) $ fun2 i cands
        -- numbers contained in the pairs
        ns = nub $ concat pairs
        -- remove those numbers from other candidate cells
        eliminate = filter (\(c, v) -> p c && c `elem` otherCells && v `elem` ns)

-- 1) For each row/column/box, find out the cells that share
--    a triplet of numbers as their only possible solutions.
--    A cell may have 2-3 of these numbers.
-- 2) Remove these numbers from Candidates list, for each
--    row/column/box the pairs affects
eliminateTriples :: Sudoku -> Sudoku
eliminateTriples state@(Sudoku sgrid xs _) = state {candidates = nxs}
  where
    nxs = eliminator stepX xs

    stepX desc cands fun2 p i xs
      -- need more than 3 cells in order to be of any use
      | cellCount > 3 && pairlen > 0 = stepX2 desc cands triples fun2 p i xs
      | otherwise                = const []
      where
        cellCount = length $ toCells cands
        triples = getNakedTriples cands
        -- pairlen = trace ("pairlen at " ++ desc ++ " " ++ show i ++ " " ++ show triples ++ " " ++ show cellCount ++ show (length triples)) $ length triples
        pairlen = length triples

    -- more then one triple unlikely but still possible
    stepX2 _ _ [] _ _ _ _ = id
    stepX2 desc cands (triple:triples) fun2 p i xs = eliminate . stepX2 desc cands triples fun2 p i xs
      where
        subseqs = filter (between 2 3 . length) . subsequences $ triple
        otherCells = map fst . filter (flip notElem subseqs . snd) $ fun2 i cands
        ns = nub triple
        eliminate = filter (\(c, v) -> p c && c `elem` otherCells && v `elem` ns)

-- 1) For each row/column/box, find out the cells that share
--    a quad of numbers as their only possible solutions.
--    A cell may have 2-4 of these numbers.
-- 2) Remove these numbers from Candidates list, for each
--    row/column/box the pairs affects
eliminateQuads :: Sudoku -> Sudoku
eliminateQuads state@(Sudoku sgrid xs _) = state {candidates = nxs}
  where
    nxs = eliminator stepX xs

    stepX desc cands fun2 p i xs
      -- need more than 4 cells in order to be of any use
      | cellCount > 4 && pairlen > 0 = stepX2 desc cands quads fun2 p i xs
      | otherwise                = const []
      where
        cellCount = length $ toCells cands
        quads = getNakedQuads cands
        -- pairlen = trace ("pairlen at " ++ desc ++ " " ++ show i ++ " " ++ show quads ++ " " ++ show cellCount ++ show (length quads)) $ length quads
        pairlen = length quads

    -- more then one quad unlikely but still possible
    stepX2 _ _ [] _ _ _ _ = id
    stepX2 desc cands (quad:quads) fun2 p i xs = eliminate . stepX2 desc cands quads fun2 p i xs
      where
        subseqs = filter (between 2 4 . length) . subsequences $ quad
        otherCells = map fst . filter (flip notElem subseqs . snd) $ fun2 i cands
        ns = nub quad
        -- ns = trace ("foo " ++ show otherCells) (nub $ concat quads)
        eliminate = filter (\(c, v) -> p c && c `elem` otherCells && v `elem` ns)

getSingles :: [Candidate] -> [Candidate]
getSingles cands =
  concat [filter (f i cands) cands | i <- nub (map snd cands)] ++ singles
  where
    -- only one cell has the candidate
    f i xs (c, v) = length (filter (\(_, v1) -> v == v1 && v1 == i) xs) == 1

    -- cells that have only one candidate left
    singles = foldr step [] $ toCells cands

    step :: CellCoordinate -> [Candidate] -> [Candidate]
    -- for each unique cell, find number of possible values
    -- if 1, add to acc
    step c@(x, y) acc = if length (filter (g c) cands) == 1
                        then acc ++ filter ((== c) . fst) cands
                        else acc
    g c@(x, y) (c2, _) = c == c2

-- return a list of subsequences that satisfy condition p for length
seqsFun p xs = nub . filter (p . length) . subsequences $ (nub . sort) xs

-- Functions that return pairs, triples and quads for given sequence
--
getPairs :: [Int] -> [[Int]]
getPairs xs = pairs
  where
    pairs   = seqsFun (== 2) xs

getTriples :: [Int] -> ([[Int]], [[Int]], [[Int]])
getTriples xs = (pairs, triples, seqs)
  where
    seqs    = seqsFun (between 2 3) xs
    pairs   = filter ((== 2) . length) seqs
    triples = filter ((== 3) . length) seqs

getQuads :: [Int] -> ([[Int]], [[Int]], [[Int]], [[Int]])
getQuads xs = (pairs, triples, quads, seqs)
  where
    seqs    = seqsFun (between 2 4) xs
    (pairs:triples:quads:[]) = map (\x -> filter ((==) x . length) seqs) [2..4]

-- need to find which cells have two candidates
getNakedPairs :: [Candidate] -> [[Int]]
getNakedPairs cands = found
  where
    -- cells that we are interested in
    cells = toCells cands

    -- possible pairs left for elimination
    pairs = getPairs . map snd $ cands

    -- if a pair is contained to two cells
    found = filter f pairs
    f x = hitLen == 2
      where
        prs = getPairs x
        hitLen = length . filter (\(c,vals) -> isInfixOf [vals] prs) $ [getCellCA c cands | c <- cells]

-- need to find sets of three numbers
getNakedTriples :: [Candidate] -> [[Int]]
getNakedTriples cands = found
  where
    -- looking for triples like 25, 26, 56 or 26, 29, 269 or 256, 256, 256 etc.
    cells = toCells cands

    (_, triples, _) = getTriples . map snd $ cands

    found = filter f triples
    f x = hitLen == 3
      where
        (prs, trpls, bth) = getTriples x
        hitLen = length . filter (\(c, vals) -> isInfixOf [vals] bth) $ [getCellCA c cands | c <- cells]

-- need to find sets of four numbers
getNakedQuads :: [Candidate] -> [[Int]]
getNakedQuads cands = found
  where
    cells = toCells cands

    (_, _, quads, _) = getQuads . map snd $ cands

    found = filter f quads
    f x = hitLen == 4
      where
        (_, _, _, bth) = getQuads x
        hitLen = length . filter (\(c, vals) -> isInfixOf [vals] bth) $ [getCellCA c cands | c <- cells]

-- 1) For each box, find out if two cells in the same row/column
--    contain a number
-- 2) the number can be eliminated from the same row/cell in other boxes
eliminatePointingPairs :: Sudoku -> Sudoku
eliminatePointingPairs state@(Sudoku sgrid xs loki) = Sudoku ngrid nxs loki
  where
    (ngrid, nxs) = foldr (\i -> stepC i . stepR i)  (sgrid, xs) [1..9]

    stepC col (grid, xs)
      | length numbers > 2 = stepX col pnub inSameCol cands numbers (grid, xs)
      | otherwise = (grid, xs)
      where
        cands = getColC col xs
        numbers = nub . map snd $ cands
        pnub (a, _) (b, _) = a < b

    stepR row (grid, xs)
      | length numbers > 2 = stepX row pnub inSameRow cands numbers (grid, xs)
      | otherwise = (grid, xs)
      where
        cands = getRowC row xs
        numbers = nub . map snd $ cands
        pnub (_, a) (_, b) = a < b

    -- pnub helps to avoid duplicate matches for foos
    stepX line pnub psameL cands ns acc@(grid, xs) = foldr step acc ns
      where
        step n acc1
          | null ppairs	= acc1
          | otherwise   = eliminate line cands n ppairs acc1
          where
            -- pairs in the same box
            pairs = nub [(c1, v1) |
                         x@(c1, v1) <- cands, y@(c2, v2) <- cands,
                         c1 /= c2 && haveSameValue x y n,
                         pnub c1 c2,
                         psameL c1 c2,
                         c1 <#> c2 ]

            p x@(c1, v1) y@(c2, v2) = x <=> y && c1 <#> c2 && not (psameL c1 c2)
            -- pairs that are pointing pairs
            ppairs = filter (\x@(c@(i, j), v) ->
                              length (filter (not . inSameBox c . fst) (candsThatHave v cands)) > 0
                              && not (any (p x) (getBoxC (xyToBoxN i j) xs))) pairs

    eliminate line cands n foos acc@(grid, xs) = (grid, nxs)
      where
        n = head (map snd foos)
        elim = filter (\(c1, v1) ->
                        v1 == n
                        && not (any (inSameBox c1 . fst) foos)) cands
        nxs = xs \\ elim

candsThatHave :: Int -> [Candidate] -> [Candidate]
candsThatHave x = filter (\(c,v) -> v == x)

-- http://www.sudokuwiki.org/Y_Wing_Strategy
-- 1) Three cells that have only two candidates left so that
--    hing cell has AB and wing cells have AC and BC, and hinge cell
--    sees the wings.
-- 2) Candidate C can be removed from those cell that are seen by both wing
--    cells
eliminateYWings :: Sudoku -> Sudoku
eliminateYWings state@(Sudoku sgrid xs _) = state { candidates = nxs }
  where
    nxs = foldr step xs ywings

    step (w1@(a, [a1, a2]), hinge@(b, [b1, b2]), w2@(c, [c1, c2])) xs
      | a1 == b1 && b2 == c1 && a2 == c2  = xs \\ eliminate a b c a2 xs
      | a2 == b1 && b2 == c2 && a1 == c1  = xs \\ eliminate a b c a1 xs
      | otherwise = xs

    eliminate a hinge c n xs = filter ((==) n . snd) (cellsSeen hinge a c xs)

    cells = nub . map fst $ xs
    pairs = filter ((== 2) . length . snd) $ map (`getCellCA` xs) cells
    ywings = nub [ (p1, p2, p3) | p1@(c1, v1) <- pairs,
                   p2@(c2, v2) <- pairs, p3@(c3, v3) <- pairs,
                   -- need different cells
                   length (nub [c1, c2, c3]) == 3,
                   -- values must be a set of three
                   length (nub (concat [v1, v2, v3])) == 3,
                   goodWing p1 p2 p3 xs]

    cellSeen a b = a <-> b || a <|> b || a <#> b

    -- a = AC, b = AB, c = BC
    goodWing (a, []) (hinge, []) (c, []) xs = False
    goodWing (a, [a1, a2]) (hinge, [b1, b2]) (c, [c1, c2]) xs =
      cellSeen hinge a && cellSeen hinge c
      && ((a1 == b1 && b2 == c1 && a2 == c2)
          || (a2 == b1 && b2 == c2 && a1 == c1))
      && length (cellsSeen hinge a c xs) > 0

    cellsSeen hinge a c xs =  [cell | cell@(x, v) <- xs,
                               x `notElem` [a, c, hinge],
                               cellSeen a x,
                               cellSeen c x]

-- If a line has solution for number n only in two cells, that are in the
-- same box, n can be removed from other lines in that box.
eliminateBoxLineReduction :: Sudoku -> Sudoku
eliminateBoxLineReduction state@(Sudoku sgrid xs loki) = Sudoku ngrid nxs loki
  where
    (ngrid, nxs) = foldr (\i -> stepC i . stepR i)  (sgrid, xs) [1..9]

    stepC col (grid, xs)
      -- at least two numbers needed for a pair
      | length numbers >= 2 = stepX col inSameCol cands numbers (grid, xs)
      | otherwise = (grid, xs)
      where
        cands = getColC col xs
        numbers = nub . map snd $ cands

    stepR row (grid, xs)
      | length numbers >= 2 = stepX row inSameRow cands numbers (grid, xs)
      | otherwise = (grid, xs)
      where
        cands = getRowC row xs
        numbers = nub . map snd $ cands

    stepX line p1 cands ns acc@(grid, xs) = foldr step acc ns
      where
        step n acc1
          | null ppairs	= acc1
          | otherwise   = eliminate p1 n ppairs acc1
          where
            pairs = nub [(c1, v1) |
                         x@(c1, v1) <- cands,
                         y@(c2, v2) <- cands,
                         c1 /= c2,
                         haveSameValue x y n,
                         p1 c1 c2]

            p x@(c1, v1) y@(c2, v2) = x <=> y && c1 <#> c2 && not (p1 c1 c2)

            ppairs = filter (\x@(c@(i, j), v) ->
                              not (all (inSameBox c . fst) (candsThatHave v cands))
                              && any (p x) (getBoxC (xyToBoxN i j) xs)) pairs

    eliminate p1 n pairs acc@(grid, xs) = (grid, nxs)
      where
        n = head (map snd pairs)
        -- eliminate those in same box that have same value but
        -- are not in same line
        elim = filter (\(c1, v1) ->
                        v1 == n
                        && all (inSameBox c1 . fst) pairs
                        && not (any (p1 c1 . fst) pairs)) xs
        nxs = xs \\ elim

-- If two parallel lines have candidate n in exactly two places that
-- are the same in both lines (i.e., n is a candidate in the corners of a box)
-- n can be eliminated from other cells in perpendical lines, for example
--
--  n   N  N
--
--      N  N  n
--
-- -> small n can be removed
eliminateXWings :: Sudoku -> Sudoku
eliminateXWings state@(Sudoku sgrid xs loki) = Sudoku ngrid nxs loki
  where
    (ngrid, nxs) = foldr (\i -> stepC i . stepR i) (sgrid, xs) [2..9]

    stepC col (grid, xs)
      -- at least two numbers needed for a pair
      | length numbers >= 2     = stepX col getColC p inSameCol inSameRow cands numbers (grid, xs)
      | otherwise       = (grid, xs)
      where
        cands = getColC col xs
        numbers = nub . map snd $ cands
        p (a, _) (b, _) = a < b

    stepR row (grid, xs)
      -- at least two numbers needed for a pair
      | length numbers >= 2     = stepX row getRowC p inSameRow inSameCol cands numbers (grid, xs)
      | otherwise       = (grid, xs)
      where
        cands = getRowC row xs
        numbers = nub . map snd $ cands
        p (_, a) (_, b) = a < b

    -- for columns, p1 (a, _) (b, _) = a < b
    -- sameL ... same line predicate
    -- perpL ... perpendical line predicate
    stepX line f1 p1 sameL perpL cands numbers (grid, xs)
      | null wings = (grid, xs)
      | otherwise  = eliminateX perpL wings (grid, xs)
      where
        wings = concat [[(n, c1, c2, fst (head others), snd (head others))
                        | x@(c1, v1) <- cands,
                          y@(c2, v2) <- cands,
                          let others = otherLinesX f1 p1 sameL perpL x y [1..(line-1)] xs,
                          c1 /= c2, haveSameValue x y n,
                          p1 c1 c2, sameL c1 c2,
                          length others == 1,
                          length (filter ((== n) . snd) cands) == 2,
                          length (filter ((== n) . snd) xs) > 4]
                       | n <- numbers]

    otherLinesX f1 p1 sameL perpL (c1@(x1, x2), v1) (c2@(y1, y2), v2) lines xs =
      nub res
      where
        tmp = [[(a, b) |
                (a@(a1, a2), va) <- cands,
                (b@(b1, b2), vb) <- cands,
                a2 /= x2, va == v1, va == vb,
                p1 a b, sameL a b,
                perpL a c1, perpL b c2,
                null (filter ((== va) . snd) cands \\ [(a, va), (b, vb)])]
              | line <- lines, let cands = f1 line xs]
        res = concat $ filter ((== 1) . length) tmp

    -- p1 = inSameCol
    eliminateX p1 wings acc@(grid, xs) = foldr step acc wings
      where
        step (n, c1, c2, c3, c4) acc@(grid, xs) = (grid, xs \\ elim)
          where
            elim = [x | x@(c, v1) <- xs,
                    v1 == n, p1 c c1 || p1 c c2,
                    c `notElem` [c1, c2, c3, c4]]

-- http://www.sudokuwiki.org/XYZ_Wing
-- 1) Three cells that share numbers ABC so that
--    hinge cell has ABC and wing cells have AC and BC, and hinge cell
--    sees the wings.
-- 2) Candidate C can be removed from those cell that are seen by the
--    cells
eliminateXYZWings :: Sudoku -> Sudoku
eliminateXYZWings state@(Sudoku sgrid xs _) = state { candidates = nxs }
  where
    nxs = foldr step xs xyzwings

    step ((a, w1@[a1, a2]), (b, h@[b1, b2, b3]), (c, w2@[c1, c2])) xs
      = xs \\ eliminate a b c (head (intersect w1 w2)) xs

    eliminate a hinge c n xs = filter ((==) n . snd) (cellsSeen hinge a c xs)

    cells = nub . map fst $ xs
    pairs = filter (between 2 3 . length . snd) $ map (`getCellCA` xs) cells
    xyzwings = nub [ (p1, p2, p3) | p1@(c1, v1) <- pairs,
                     p2@(c2, v2) <- pairs, p3@(c3, v3) <- pairs,
                     -- need different cells
                     length (nub [c1, c2, c3]) == 3,
                     -- values must be a set of three
                     length (nub (concat [v1, v2, v3])) == 3,
                     goodWing p1 p2 p3 xs]

    cellSeen a b = a <-> b || a <|> b || a <#> b

    -- a = AC, b = ABC, c = BC
    goodWing (a, []) (hinge, []) (c, []) xs = False
    goodWing (a, w1@[a1, a2]) (hinge, h@[b1, b2, b3]) (c, w2@[c1, c2]) xs =
      length (intersect h w1) == 2 && length (intersect h w2) == 2
      && length (intersect w1 w2) == 1
      && cellSeen hinge a && cellSeen hinge c
      && length (cellsSeen hinge a c xs) > 0

    goodWing (a, _) (hinge, _) (c, _) _ = False

    cellsSeen hinge a c xs =  [cell | cell@(x, v) <- xs,
                               x `notElem` [a, c, hinge],
                               cellSeen a x,
                               cellSeen hinge x,
                               cellSeen c x]

strategies = [("singles", eliminateSingles),
              ("naked pairs", eliminatePairs),
              ("naked triples", eliminateTriples),
              ("naked quads", eliminateQuads),
              ("pointing pairs", eliminatePointingPairs),
              ("box line reduction", eliminateBoxLineReduction),
              ("X-wing", eliminateXWings),
              ("Y-wing", eliminateYWings),
              ("XYZ-wing", eliminateXYZWings)]

-- |Function 'solve' attempts to solve the given sudoku puzzle
solve :: Sudoku -> Sudoku
solve state@(Sudoku sgrid xs _) = solvestep strategies state

-- step through strategies
solvestep :: [(String, Sudoku -> Sudoku)] -> Sudoku -> Sudoku
solvestep [] state = state
solvestep ((fdesc,f):fs) state@(Sudoku sgrid xs loki)
  | success     = solvestep strategies nstate { loki = nloki }
  | otherwise   = solvestep fs nstate
  where
    nstate@(Sudoku nsgrid nxs _) = f state
    success     = xs /= nxs
    nloki   = addToLog fdesc (xs \\ nxs) (gridDiff2 sgrid nsgrid) loki

    addToLog fdesc xs grid loki =
      ("Solved by " ++  fdesc ++ ": " ++ show grid ++ "\n\teliminated: " ++ show xs ++ "\n") : loki

-- difference of grids
gridDiff :: SudokuGrid -> SudokuGrid -> SudokuGrid
gridDiff a b = A.listArray ((1,1),(9,9)) (diff (A.elems a) (A.elems b))
  where
    diff [] [] = []
    diff (a:as) []     = []
    diff []     (b:bs) = []
    diff (a:as) (b:bs)
      | a == b = Unsolved : diff as bs
      | otherwise = b : diff as bs

gridDiff2 :: SudokuGrid -> SudokuGrid -> [Candidate]
gridDiff2 a b = diff (A.assocs a) (A.assocs b)
  where
    diff :: [(CellCoordinate, Cell Int)] -> [(CellCoordinate, Cell Int)] -> [Candidate]
    diff [] [] = []
    diff (a:as) []     = []
    diff []     (b:bs) = []
    diff (a:as) (b@(c, v):bs)
      | a == b    = diff as bs
      | otherwise = (c, cellNum v) : diff as bs

-- more generic helper funs
between i j x = i <= x && x <= j

erase :: (Eq a) => a -> [a] -> [a]
erase _ [] = []
erase a (x:xs) | x == a = erase a xs
               | otherwise = x : erase a xs

eraseBy :: (Eq a) => (a -> Bool) -> [a] -> [a]
eraseBy _ [] = []
eraseBy p (x:xs) | p x       = eraseBy p xs
                 | otherwise = x : eraseBy p xs

eraseAllBy :: (Eq a) => (a -> a-> Bool) -> [a] -> [a] -> [a]
eraseAllBy p xs ys = xs \\ [x | x <- xs, y <- ys, p x y]

-- eraseBy p = snd . partition p

main =
  do
    -- grid  <- readFile "sudoku.txt"
    -- let grid = "014600300 050000007 090840100 000400800 600050009 007009000 008016030 300000010 009008570"
    let grid = "090060500080950001000803000204000080060080010010000702000509000100027090007010040"
    -- let grid = grid1
    putStrLn (prettyPrint $ convert grid)

    let state@(Sudoku sgrid xs _) = newSolver (convert grid)
    -- putStrLn $ prettyPrintCandidates (getBoxC 4 xs)
    let nstate@(Sudoku nsgrid nxs loki) = solve state
    -- putStrLn $ prettyPrint (gridDiff sgrid nsgrid)
    putStrLn $ (concat . reverse) loki
    putStrLn $ prettyPrint nsgrid
    -- print nxs
    putStrLn $ prettyPrintCandidates nxs

-- grid1 needed y-wing
grid1 = "014600300 050000007 090840100 000400800 600050009 007009000 008016030 300000010 009008570"
-- grid2 needed boxlinereduction
grid2 = "200068050008002000560004801000000530400000002097000000804300096000800300030490007"
-- grid3 needed x-wing
grid3 = "700600008800030000090000310006740005005806900400092100087000020000060009600008001"
grid4 = "100060027360000000540007000900001050700050003010300002000900016000000074670040009"
grid5 = "100062900000750004003100500200000800090000060004000007009001200400075000002840005"
grid6 = "600510009891704305205800041300147206100256038062938004010400003903671402426385007"
grid7 = "005000800080030004203700000307086000500402007000390205000009502900070060001000300"
grid8 = "600590000081007000000603200005100090018402670030006100004309000000700910000015002"
-- Solved by XYZ-wing: [], eliminated: [((4,3),8)]
grid9 = "000704005020010070000080002090006250600070008053200010400090000030060090200407000"
