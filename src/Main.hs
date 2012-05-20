module Main where
import Data.List
import Sudoku
import SudokuParser
import Text.ParserCombinators.Parsec

-- Read the sudoku file from standard input
main :: IO ()
main =
    do c <- getContents
       case parse parseMatrix "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> solveIO (convert r) 

-- Helper method
solveIO :: [CellCoordinate] -> IO ()
solveIO sudoku = do 
        putStrLn "Solving sudoku:"
        putStrLn (pretty (sortBy sortCell (sudoku ++ supplement sudoku)))
        let solved = sortBy sortCell $ solveSudoku sudoku
        case solved of
                [] -> putStrLn "Unfortunately no solution is possible for this sudoku"
                _  -> putStrLn ("Found solution: \n" ++ pretty solved)

convertRow :: [Int] -> [Int] -> Int -> [CellCoordinate]
convertRow _  [] _ = []
convertRow  (c:rc) (r:rs) rowIndex | r == 0 = ys
                                   | otherwise = y : ys  where y  = (r, rowIndex, c)
                                                               ys = convertRow  rc rs rowIndex
convert :: [[Int]] -> [CellCoordinate]
convert xs  = convert' values  xs values

convert' :: [Int] -> [[Int]] -> [Int] -> [CellCoordinate]
convert' [] [] _ = []
convert' v r c = convertRow c (head r) (head v) ++ convert' (tail v) (tail r) c 

-- Print out a (solved) Sudoku in human readable form. 
pretty :: [CellCoordinate] -> String
pretty [] = ""
pretty xs = prettyRow row  ++ "\n" ++ pretty rest where row = take size xs
                                                        rest =drop size xs

prettyRow :: [CellCoordinate] -> String
prettyRow xs = show values where values = map value xs
                                 value (v,_,_) = v
                                 
-- Ordering for CelCoordinates to be able to sort the solution by row and column                               
sortCell :: CellCoordinate -> CellCoordinate -> Ordering
sortCell (_,r1,c1) (_,r2,c2) | r1 < r2 || (r1==r2 && c1 < c2)= LT
                             | r1 > r2 || (r1==r2 && c1>c2) = GT
                             | otherwise = EQ
  
-- Supplement the given cell coordinates with blanks
supplement :: Num t1 => [(t, Int, Int)] -> [(t1, Int, Int)]
supplement given = [(0,r,c) | r <- values, c <- values, (r, c) `notElem` givenPositions] where toRc (_,r,c) = (r,c)
                                                                                               givenPositions = map toRc given
                               