--
-- Author: Jani J. Hakala <jjhakala@gmail.com>, Jyväskylä, Finland
--
-- http://en.wikipedia.org/wiki/Sudoku
-- 9x9 grid with numbers 1-9, 3x3 sub-grids. Each number may occur once
-- in a row, column and sub-grid

-- Mark unsolved cells as 0, solved as [1,9]
-- Go through strategies, if a strategy reduces number of candidates, 
-- restart starting from simple strategies
--
-- like in this sudoku solver, http://www.sudokuwiki.org/sudoku.htm
--
-- Notes / Todo:
-- - quite many list comprehensions, could probably have same 
--   row/column/box traversal routines
-- - generalise getNakedPairs / getNakedTriples / getNakedQuads etc.
-- - eliminateSingles might need fixing ('stepB' makes things go bad)
-- - better data structures for the task?
-- - check for proper partial / whole solutions, abort early if not
-- - should probably have smaller functions
-- - needed to google about trace / traceShow
--
-- solve http://www.dailysudoku.com/sudoku/play.shtml?year=2013&month=09&day=19

{-
 Strategies:
  1) single possible in a row / column / sub-grid      
    -> solved cell
  2) two cells in the same column / row and in same sub-grid
  3) triplets, quads
  4) hidden pairs, triplets, quads
-}

module Main where

import Data.Array(Array,(!))
import qualified Data.Array as A
import Data.List
import Data.Char

import Debug.Trace
import Control.Arrow (second)

type SudokuGrid = Array (Int, Int) Int
type Candidates = [((Int,Int),Int)]

data SudokuState = Sudoku { grid :: SudokuGrid, 
                            candidates :: Candidates, 
                            loki :: [String] }
                  deriving (Eq, Show)
                         
-- Convert a string into a grid.
convert :: String -> SudokuGrid
convert str = A.listArray ((1,1),(9,9)) [read [xxx !! i] :: Int| i <- [0..80]]
  where xxx = filter isDigit str

newState :: SudokuGrid -> SudokuState
newState grid = Sudoku { grid = grid, 
                         candidates = findCandidates grid,  
                         loki = [] }

findCandidates :: SudokuGrid -> Candidates
findCandidates grid = concat [findCandidatesAt grid x y 
                             | x <- [1..9], y <- [1..9], grid ! (x,y) == 0]

findCandidatesAt :: SudokuGrid -> Int -> Int -> Candidates
findCandidatesAt grid x y = 
  deleteFirstsBy (\(_,v1) (_,v2) -> v1 == v2)
  [((x,y), i) | i <- [1..9]] ((nub . sort) 
                          (solvedInRow grid x ++ 
                           solvedInColumn grid y ++
                           solvedInBox grid x y))
  
solvedInRow :: SudokuGrid -> Int -> Candidates
solvedInRow    grid x = [((x, i), grid ! (x, i)) | i <- [1..9],
                         grid ! (x, i) > 0]

solvedInColumn :: SudokuGrid -> Int -> Candidates
solvedInColumn grid y = [((i, y), grid ! (i, y)) | i <- [1..9],
                         grid ! (i, y) > 0]

solvedInBox :: SudokuGrid -> Int -> Int -> Candidates
solvedInBox    grid x y = [((i, j), grid ! (i, j))
                          | i <- [i0..i1], j <- [j0..j1], grid ! (i,j) > 0]
  where
    ((i0, j0), (i1, j1)) = xyToBox x y

-- Convert a grid to printable string.
prettyPrint :: SudokuGrid -> String
prettyPrint bg = unlines [ intersperse ' ' 
                           (concat [show (bg ! (row,col)) | col <- [y0..y1]])
                         | row <- [x0..x1]]
  where 
    ((x0, y0), (x1, y1)) = A.bounds bg
               
-- Should do this box by box?
prettyPrintCandidates :: Candidates -> String
prettyPrintCandidates xs = intercalate "\n" $
                           map (\((x, y), v) -> 
                                 (intercalate "," (map show [x, y, v]))) xs
                           
-- 3x3 box helpers
boxes = [((i, j), (i+2, j+2)) | i <- [1,4,7], j <- [1,4,7]]
boxBounds i = boxes !! (i - 1)

xyToBox :: Int -> Int -> ((Int,Int), (Int, Int))
xyToBox x y = head (filter (\((x1,y1), (x2,y2)) -> 
                             and [x1 <= x, x2 >= x, 
                                  y1 <= y, y2 >= y]) boxes)

getCellC :: (Int, Int) -> Candidates -> Candidates
getCellC (i, j) = filter (\((x, y), v) -> x == i && y == j)

getCellCA :: (Int, Int) -> Candidates -> ((Int, Int), [Int])
getCellCA (i, j) cands = (,) (i, j) (map snd (filter (\((x, y), _) -> x == i && y == j) cands))

getCA :: [((Int,Int), [Int])] -> Candidates -> [((Int, Int), [Int])]
getCA prev [] = map (second reverse) prev
getCA [] ((cell,v):xs) = getCA [(cell, [v])] xs
getCA prev@((ycell, yvals):ys) ((xcell, xval):xs)
  | xcell == ycell  = getCA ((ycell, xval : yvals) : ys) xs
  | otherwise  	= getCA ((xcell, [xval]) : prev) xs

getRowCA :: Int -> Candidates -> [((Int, Int), [Int])]
getRowCA i = getCA [] . getRowC i
    
getColCA :: Int -> Candidates -> [((Int, Int), [Int])]
getColCA i = getCA [] . getColC i

getBoxCA :: Int -> Candidates -> [((Int, Int), [Int])]
getBoxCA i = getCA [] . getBoxC i

getRowC :: Int -> Candidates -> Candidates
getRowC i = filter (\((x, y), v) -> x == i)

getColC :: Int -> Candidates -> Candidates
getColC i = filter (\((x, y), v) -> y == i)

getBoxC :: Int -> Candidates -> Candidates
getBoxC i = filter (f i)
  where 
    f box ((r, c), v) = boxBounds i == xyToBox r c

eliminateSingles :: SudokuState -> SudokuState
eliminateSingles state@(Sudoku sgrid xs loki) = 
  -- trace ("eliminateSingles: " ++ show (xs \\ updateCandidates ngrid xs)) 
  Sudoku ngrid nxs loki
  where
    (ngrid, nxs) = foldr stepC (foldr stepR (sgrid, xs) [1..9]) [1..9]
    -- stepB somehow broken
    
    stepR row (grid, xs) = (ngrid, nxs) 
      where 
        singles = getSingles (getRowC row xs)
        ngrid = grid A.// singles
        nxs = (updateBox singles . updateRowsCols singles . updateGrid ngrid) xs
    
    -- (trace ("asdffff " ++ show (getSingles (getRowC row xs))) (getSingles (getRowC row xs)))
    
    stepC col (grid, xs) = (ngrid, nxs)
      where
        singles = getSingles (getColC col xs)
        ngrid = grid A.// singles
        nxs = (updateBox singles . updateRowsCols singles . updateGrid ngrid) xs
    
    stepB box (grid, xs) = (ngrid, nxs) 
      where 
        singles = getSingles (getBoxC box xs)
        ngrid = grid A.// singles
        nxs = (updateBox singles . updateRowsCols singles . updateGrid ngrid) xs
    
    -- only updates solved cells.
    updateGrid grid xs = 
      foldr (\ (c1@(x1, y1), v1) acc -> 
              eraseBy (\(c2@(x2, y2), v2) -> 
                        c1 == c2 && v1 /= 0) acc) xs (A.assocs grid)
      
    updateRowsCols singles xs = foldr step xs singles
      where
        step (c1@(x1,y1), v1) = 
          eraseBy (\(c2@(x2, y2), v2) -> 
                    (c1 /= c2 && v1 == v2) && (x1 == x2 || y1 == y2))
      
    updateBox singles xs = foldr step xs singles
      where
        step (c1@(x1, y1), v1) = 
          eraseBy (\(c2@(x2, y2), v2) -> 
                    (c1 /= c2 
                     && xyToBox x1 y1 == xyToBox x2 y2 
                     && v1 == v2))

updateBox singles xs = foldr step xs singles
  where
    step (c1@(x1,y1), v1) = 
      eraseBy (\(c2@(x2, y2), v2) -> 
                (c1 /= c2 
                 && xyToBox x1 y1 == xyToBox x2 y2 
                 && v1 == v2))

eliminatePairs :: SudokuState -> SudokuState
eliminatePairs state@(Sudoku sgrid xs loki) = state {candidates = nxs}
  where
    nxs = foldr stepB (foldr stepC (foldr stepR xs [1..9]) [1..9]) [1..9]
    -- nxs = foldr stepC (foldr stepR xs [1..9]) [1..9]
    
    stepX fun1 fun2 p i xs
      -- need more than 2 cells in order to be of any use
      | cellCount > 2 && pairlen > 0	= eliminate
      | otherwise 			= const []
      where
        cells = fun1 i xs
        cellCount = length . nub . map fst $ cells
        pairs = getNakedPairs cells
        pairlen = length pairs
        otherCells = (map fst . filter (\(cell, v) -> v `notElem` pairs)) (fun2 i cells)
        ns = nub $ concat pairs
        eliminate = filter (\(cell@(r,c), v) -> p (r, c) && (r,c) `elem` otherCells && v `elem` ns)

    stepR row acc0 	= acc0 \\ eliminate acc0
      where
        eliminate = stepX getRowC getRowCA (\(r,c) -> r == row) row acc0

    stepC col acc0 	= acc0 \\ eliminate acc0
      where
        eliminate = stepX getColC getColCA (\(r,c) -> c == col) col acc0
        
    stepB box acc0 	= acc0 \\ eliminate acc0
      where
        eliminate = stepX getBoxC getBoxCA (\(r,c) -> xyToBox r c == boxBounds box) box acc0

eliminateTriples :: SudokuState -> SudokuState
eliminateTriples state@(Sudoku sgrid xs loki) = state {candidates = nxs}
  where
    nxs = foldr stepB (foldr stepC (foldr stepR xs [1..9]) [1..9]) [1..9]
    -- nxs = foldr stepC (foldr stepR xs [1..9]) [1..9]
    
    stepX desc fun1 fun2 p i xs
      -- need more than 3 cells in order to be of any use
      | cellCount > 3 && pairlen > 0 = stepX2 desc cells triples fun2 p i xs
      | otherwise     		     = const []
      where
        cells = fun1 i xs
        cellCount = length . nub . map fst $ cells
        triples = getNakedTriples cells
        -- pairlen = trace ("pairlen at " ++ desc ++ " " ++ show i ++ " " ++ show triples ++ " " ++ show cellCount ++ show (length triples)) $ length triples
        pairlen = length triples

    stepX2 desc cells triples fun2 p i xs = eliminate
      where
        subseqs = filter (between 2 3 . length) . subsequences $ head triples
        otherCells = (map fst . filter (\(cell, v) -> v `notElem` subseqs)) (fun2 i cells)
        ns = nub $ concat triples -- trace ("foo " ++ show otherCells) (nub $ concat triples)
        eliminate = filter (\(cell@(r,c), v) -> p (r, c) && (r,c) `elem` otherCells && v `elem` ns)

    stepR row acc0 	= acc0 \\ eliminate acc0
      where
        eliminate = stepX "row" getRowC getRowCA (\(r,c) -> r == row) row acc0

    stepC col acc0 	= acc0 \\ eliminate acc0
      where
        eliminate = stepX "col" getColC getColCA (\(r,c) -> c == col) col acc0
        
    stepB box acc0 	= acc0 \\ eliminate acc0
      where
        eliminate = stepX "box" getBoxC getBoxCA (\(r,c) -> xyToBox r c == boxBounds box) box acc0

eliminateQuads :: SudokuState -> SudokuState
eliminateQuads state@(Sudoku sgrid xs loki) = state {candidates = nxs}
  where
    nxs = foldr stepB (foldr stepC (foldr stepR xs [1..9]) [1..9]) [1..9]
    stepX desc fun1 fun2 p i xs
      -- need more than 4 cells in order to be of any use
      | cellCount > 4 && pairlen > 0 = stepX2 desc cells quads fun2 p i xs
      | otherwise     		     = const []
      where
        cells = fun1 i xs
        cellCount = length . nub . map fst $ cells
        quads = getNakedQuads cells
        -- pairlen = trace ("pairlen at " ++ desc ++ " " ++ show i ++ " " ++ show quads ++ " " ++ show cellCount ++ show (length quads)) $ length quads
        pairlen = length quads

    stepX2 desc cells quads fun2 p i xs = eliminate
      where
        subseqs = filter (between 2 4 . length) . subsequences $ head quads
        otherCells = (map fst . filter (\(cell, v) -> v `notElem` subseqs)) (fun2 i cells)
        ns = nub $ concat quads
        -- ns = trace ("foo " ++ show otherCells) (nub $ concat quads)
        eliminate = filter (\(cell@(r,c), v) -> p (r, c) && (r,c) `elem` otherCells && v `elem` ns)

    stepR row acc0 	= acc0 \\ eliminate acc0
      where
        eliminate = stepX "row" getRowC getRowCA (\(r,c) -> r == row) row acc0

    stepC col acc0 	= acc0 \\ eliminate acc0
      where
        eliminate = stepX "col" getColC getColCA (\(r,c) -> c == col) col acc0
        
    stepB box acc0 	= acc0 \\ eliminate acc0
      where
        eliminate = stepX "box" getBoxC getBoxCA (\(r,c) -> xyToBox r c == boxBounds box) box acc0


getSingles :: Candidates -> Candidates
getSingles cands = 
  -- only one cell has the candidate
  concat [filter (f i cands) cands | i <- [1..9]]
  ++ foos 
  where 
    f i xs (c, v) = length (filter (\(_, v1) -> v == v1 && v1 == i) xs) == 1
                    
    foos = foldr step [] (nub (map fst cands))

    step :: (Int, Int) -> Candidates -> Candidates
    -- for each unique cell, find number of possible values
    -- if 1, add to acc
    step c@(x, y) acc = if length (filter (g c) cands) == 1
                        then acc ++ filter ((== c) . fst) cands
                        else acc
    g c@(x, y) (c2, _) = c == c2
                    
seqsFun p xs = cleanFun p . subsequences $ (nub . sort) xs
  where
    cleanFun p = sort . nub . filter (p . length) . map (nub . sort)

getPairs :: [Int] -> [[Int]]
getPairs xs = pairs
  where
    pairs   = seqsFun (== 2) xs

getTriples :: [Int] -> ([[Int]], [[Int]], [[Int]])
getTriples xs = (pairs, triples, both)
  where
    seqs    = seqsFun (>= 2) xs
    pairs   = filter ((== 2) . length) seqs 
    triples = filter ((== 3) . length) seqs
    both    = sort $ union pairs triples

getQuads :: [Int] -> ([[Int]], [[Int]], [[Int]], [[Int]])
getQuads xs = (pairs, triples, quads, alt)
  where
    seqs    = seqsFun (>= 2) xs
    pairs   = filter ((== 2) . length) seqs 
    triples = filter ((== 3) . length) seqs
    quads   = filter ((== 4) . length) seqs
    alt     = sort $ union (pairs `union` triples) quads

between i j x = i <= x && x <= j

-- need to find which cells have two candidates
getNakedPairs :: Candidates -> [[Int]]
getNakedPairs cands = found
  where 
    cells = nub (map fst cands)
    
    pairs = getPairs . map snd $ cands
    
    found = filter f pairs
    f x = hitLen == 2
      where
        prs = getPairs x
        hitLen = length . filter (\(c,vals) -> isInfixOf [vals] prs) $ [getCellCA c cands | c <- cells]

getNakedTriples :: Candidates -> [[Int]]
getNakedTriples cands = found
  where 
    -- looking for triples like 25, 26, 56 or 26, 29, 269 or 256, 256, 256 etc.
    cells = nub (map fst cands)
    
    (_, triples, _) = getTriples . map snd $ cands
    
    found = filter f triples
    f x = hitLen == 3
      where
        (prs, trpls, bth) = getTriples x
        hitLen = length . filter (\(c,vals) -> isInfixOf [vals] bth) $ [getCellCA c cands | c <- cells]
              
getNakedQuads :: Candidates -> [[Int]]
getNakedQuads cands = found
  where 
    cells = nub (map fst cands)
    
    (_, _, quads, _) = getQuads . map snd $ cands
    
    found = filter f quads
    f x = hitLen == 4
      where
        (_, _, _, bth) = getQuads x
        hitLen = length . filter (\(c,vals) -> isInfixOf [vals] bth) $ [getCellCA c cands | c <- cells]


solve :: SudokuState -> SudokuState
-- step through strategies
solve state@(Sudoku sgrid xs _) = solveSingles state True

-- TODO: replace with a single function that loops through a set of solvers?
solveSingles :: SudokuState -> Bool -> SudokuState
solveSingles state False = state
solveSingles state@(Sudoku sgrid xs _) progress 
  | xs == nxs = plainPairs (nstate {loki = "No changes at solveSingles.\n" : nloki}) True
  | otherwise = trace ("solveSingles xs /= nxs, solved / eliminated " ++ show (xs \\ nxs)) (solveSingles (nstate {loki = "Success at solveSingles.\n" : nloki}) True)
  where 
    nstate@(Sudoku nsgrid nxs nloki) = eliminateSingles state

plainPairs :: SudokuState -> Bool -> SudokuState
plainPairs state False = state
plainPairs state@(Sudoku sgrid xs loki) progress 
  | xs == nxs = plainTriples nstate {loki = "No changes at plainPairs\n" : nloki} True
  | otherwise = trace ("plainPairs xs /= nxs, eliminated " ++ show (xs \\ nxs)) (solveSingles (nstate {loki = "Some success at plainPairs.\n" : nloki}) True)
  where 
    nstate@(Sudoku nsgrid nxs nloki) = eliminatePairs state
    
plainTriples :: SudokuState -> Bool -> SudokuState
plainTriples state False = state
plainTriples state@(Sudoku sgrid xs loki) progress 
  | xs == nxs = plainQuads nstate {loki = "No changes at plainTriples.\n" : nloki} True
  | otherwise = trace ("plainTriples xs /= nxs, eliminated " ++ show (xs \\ nxs)) (solveSingles (nstate {loki = "Some success at plainTriples.\n" : nloki}) True)
  where 
    nstate@(Sudoku nsgrid nxs nloki) = eliminateTriples state

plainQuads :: SudokuState -> Bool -> SudokuState
plainQuads state False = state
plainQuads state@(Sudoku sgrid xs loki) progress 
  | xs == nxs = nstate {loki = "No changes at plainQuads.\n" : nloki}
  | otherwise = trace ("plainQuads xs /= nxs, eliminated " ++ show (xs \\ nxs)) (solveSingles (nstate {loki = "Some success at plainQuads.\n" : nloki}) True)
  where 
    nstate@(Sudoku nsgrid nxs nloki) = eliminateQuads state

-- difference of grids
gridDiff :: SudokuGrid -> SudokuGrid -> SudokuGrid
gridDiff a b = A.listArray ((1,1),(9,9)) (diff (A.elems a) (A.elems b))
  where 
    diff [] [] = []
    diff (a:as) []     = []
    diff [] 	(b:bs) = []
    diff (a:as) (b:bs) 
      | a == b = 0 : diff as bs
      | otherwise = b : diff as bs
    
erase :: (Eq a) => a -> [a] -> [a]      
erase _ [] = []                         
erase a (x:xs) | x == a = erase a xs                  
               | otherwise = x : erase a xs

eraseBy :: (Eq a) => (a -> Bool) -> [a] -> [a]      
eraseBy _ [] = []                         
eraseBy p (x:xs) | p x 	     = eraseBy p xs 
                 | otherwise = x : eraseBy p xs

main = 
  do
    -- grid  <- readFile "sudoku.txt"
    let grid = "014600300 050000007 090840100 000400800 600050009 007009000 008016030 300000010 009008570"
    putStrLn (prettyPrint $ convert grid)

    let state@(Sudoku sgrid xs _) = newState (convert grid)
    -- print xs 
    let nstate@(Sudoku nsgrid nxs loki) = solve state
    putStrLn $ prettyPrint nsgrid
    putStrLn $ prettyPrint (gridDiff sgrid nsgrid)
    -- putStrLn $ (concat . reverse) loki
    -- print nxs 
    -- putStrLn $ prettyPrintCandidates nxs

test2 :: Candidates
test2 = [((1,6),2),((1,6),5),((1,9),2),((1,9),5),((3,6),2),((3,6),5),((3,8),2),((3,8),5),((3,8),6),((3,9),2),((3,9),5),((3,9),6),((4,2),2),((4,2),3),((4,3),1),((4,3),2),((4,3),5),((4,8),2),((4,8),5),((4,9),1),((4,9),3),((5,3),1),((5,3),2),((5,4),1),((5,4),2),((6,1),4),((6,1),5),((6,2),2),((6,2),3),((6,2),4),((6,4),1),((6,4),2),((6,7),2),((6,7),6),((6,8),2),((6,8),5),((6,8),6),((6,9),1),((6,9),3),((7,1),4),((7,1),5),((7,2),2),((7,2),4),((7,2),7),((7,4),5),((7,4),7),((7,9),2),((7,9),4),((8,2),2),((8,2),6),((8,2),7),((8,3),2),((8,3),5),((8,4),5),((8,4),7),((8,7),2),((8,7),6),((9,2),4),((9,2),6),((9,9),4),((9,9),6)]

test3 :: Candidates
test3 = [((1,1),2),((1,1),7),((1,1),8),((1,5),2),((1,5),7),((1,5),9),((1,6),2),((1,6),5),((1,6),7),((1,8),2),((1,8),5),((1,8),8),((1,8),9),((1,9),2),((1,9),5),((1,9),8),((2,1),2),((2,1),8),((2,3),2),((2,3),3),((2,3),6),((2,4),1),((2,4),2),((2,4),3),((2,4),9),((2,5),2),((2,5),3),((2,5),9),((2,6),1),((2,6),2),((2,6),3),((2,7),2),((2,7),4),((2,7),6),((2,7),9),((2,8),2),((2,8),4),((2,8),6),((2,8),8),((2,8),9),((3,1),2),((3,1),7),((3,3),2),((3,3),3),((3,3),6),((3,6),2),((3,6),3),((3,6),5),((3,6),7),((3,8),2),((3,8),5),((3,8),6),((3,9),2),((3,9),5),((3,9),6),((4,1),1),((4,1),2),((4,1),5),((4,1),9),((4,2),2),((4,2),3),((4,3),1),((4,3),2),((4,3),3),((4,3),5),((4,5),2),((4,5),3),((4,5),6),((4,5),7),((4,6),1),((4,6),2),((4,6),3),((4,6),7),((4,8),2),((4,8),5),((4,8),6),((4,9),1),((4,9),2),((4,9),3),((4,9),5),((4,9),6),((5,2),2),((5,2),3),((5,2),4),((5,2),8),((5,3),1),((5,3),2),((5,3),3),((5,4),1),((5,4),2),((5,4),3),((5,4),7),((5,6),1),((5,6),2),((5,6),3),((5,6),7),((5,7),2),((5,7),4),((5,7),7),((5,8),2),((5,8),4),((6,1),1),((6,1),2),((6,1),4),((6,1),5),((6,1),8),((6,2),2),((6,2),3),((6,2),4),((6,2),8),((6,4),1),((6,4),2),((6,4),3),((6,5),2),((6,5),3),((6,5),6),((6,5),8),((6,7),2),((6,7),4),((6,7),6),((6,8),2),((6,8),4),((6,8),5),((6,8),6),((6,9),1),((6,9),2),((6,9),3),((6,9),4),((6,9),5),((6,9),6),((7,1),2),((7,1),4),((7,1),5),((7,1),7),((7,2),2),((7,2),4),((7,2),7),((7,4),2),((7,4),5),((7,4),7),((7,4),9),((7,7),2),((7,7),4),((7,7),9),((7,9),2),((7,9),4),((8,2),2),((8,2),4),((8,2),6),((8,2),7),((8,3),2),((8,3),5),((8,3),6),((8,4),2),((8,4),5),((8,4),7),((8,4),9),((8,5),2),((8,5),7),((8,5),9),((8,6),2),((8,6),4),((8,6),5),((8,6),7),((8,7),2),((8,7),4),((8,7),6),((8,7),9),((8,9),2),((8,9),4),((8,9),6),((8,9),8),((9,1),1),((9,1),2),((9,1),4),((9,2),2),((9,2),4),((9,2),6),((9,4),2),((9,4),3),((9,5),2),((9,5),3),((9,9),2),((9,9),4),((9,9),6)]  
  
test =
  do
    print $ getNakedTriples [((5,7), 2), ((5,7), 7),
                             ((6,7), 2), ((6,7), 6),
                             ((7,7), 2), ((7,7), 9),
                             ((8,7), 2), ((8,7), 6), ((8,7), 9)]

    print $ getTriples . map snd $ [((5,7), 2), ((5,7), 7),
                                    ((6,7), 2), ((6,7), 6),
                                    ((7,7), 2), ((7,7), 9),
                                    ((8,7), 2), ((8,7), 6), ((8,7), 9)]

    print $ getTriples . map snd $ [((6,1),1),((6,1),2),((6,1),4),((6,1),5),((6,1),8),
                                    ((6,2),2),((6,2),3),((6,2),4),((6,2),8),
                                    ((6,4),1),((6,4),2),((6,4),3),
                                    ((6,5),2),((6,5),3),((6,5),6),((6,5),8),
                                    ((6,7),2),((6,7),4),((6,7),6),
                                    ((6,8),2),((6,8),4),((6,8),5),((6,8),6),
                                    ((6,9),1),((6,9),2),((6,9),3),((6,9),4),
                                    ((6,9),5),((6,9),6)]
      
  -- [((7,1),4),((7,1),5),((7,2),2),((7,2),4),((7,2),7),((8,2),2),((8,2),4),((8,2),6),((8,2),7),((8,3),2),((8,3),5),((8,3),6),((9,2),4),((9,2),6)]

{-
+-------+-------+-------+
| . 1 4 | 6 . . | 3 . . |
| . 5 . | . . . | . . 7 |
| . 9 . | 8 4 . | 1 . . |
+-------+-------+-------+
| . . . | 4 . . | 8 . . |
| 6 . . | . 5 . | . . 9 |
| . . 7 | . . 9 | . . . |
+-------+-------+-------+
| . . 8 | . 1 6 | . 3 . |
| 3 . . | . . . | . 1 . |
| . . 9 | . . 8 | 5 7 . |
+-------+-------+-------+

after singles elimination

+-------+-------+-------+
| 8 1 4 | 6 7 . | 3 9 . |
| 2 5 6 | . . . | 4 8 7 |
| 7 9 . | 8 4 . | 1 . . |
+-------+-------+-------+
| 9 . . | 4 6 7 | 8 . . |
| 6 8 . | . 5 . | 7 4 9 |
| . . 7 | . 8 9 | . . . |
+-------+-------+-------+
| . . 8 | . 1 6 | . 3 . |
| 3 . . | . . 4 | . 1 8 |
| 1 . 9 | . . 8 | 5 7 . |
+-------+-------+-------+

after plain pairs

+-------+-------+-------+
| 8 1 4 | 6 7 . | 3 9 . |
| 2 5 6 | 9 3 1 | 4 8 7 |
| 7 9 3 | 8 4 . | 1 . . |
+-------+-------+-------+
| 9 . . | 4 6 7 | 8 . . |
| 6 8 . | . 5 3 | 7 4 9 |
| . . 7 | . 8 9 | . . . |
+-------+-------+-------+
| . . 8 | . 1 6 | 9 3 . |
| 3 . . | . 9 4 | . 1 8 |
| 1 . 9 | 3 2 8 | 5 7 . |
+-------+-------+-------+


The actual solution
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

