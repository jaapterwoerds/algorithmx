module Sudoku(solveSudoku,CellCoordinate, size, values) where
import ExactCover
import Data.List

-- Represent a sudoku as exact cover which can be solved using DLX
-- The number of elements in a row, column and region

-- Explanation of constraints: http://code.google.com/p/narorumo/wiki/SudokuDLX
{--
Row-Column Constraints
The first 81 columns of a row (representing an assignment) describe the row and column that it fills in -- we describe this as rowR colC where R and C are the row and column in the sudoku puzzle. So if the assignment is for row 3, column 5, then it gets a '1' for constraint row3 col5, and a 0 for each other row and column -- it doesn't satisfy those constraints.

Row-Number, Column-Number and Box-Number Constraints
In the next 81 columns of the DLX row, we'll describe on which row the assignment falls, and what number it assigns. Much like the Row-Column constraints, we can write this as rowR numN. Then we'll do the analogous thing for Column-Number and Box-Number constraints -- we'll write down which row (or column or box) an assignment describes, and which number it assigns. And that handles the other three types of constraints, and the second, third and fourth 81-column sections.


For a new sudoku: transform $ toExactCover gives the matrix, cover this with the given initial positions and then do solve on this reduced matrix

ex = [
      "    6  8 ",
      " 2       ",
      "  1      ",
      " 7    1 2",
      "5   3    ",
      "      4  ",
      "  42 1   ",
      "3  7  6  ",
      "       5 "
   ]


toRep xs=zip xs values

ex= [(6,1,5),(8,1,8),(2,2,2),(1,3,3),(7,4,2),(1,4,7),(2,4,9),(5,5,1),(3,5,5),(4,6,7),(4,7,3),(2,7,4),(1,7,6),(3,8,1),(7,8,4),(6,8,7),(5,9,8)   ]
numberOfRowsInExactCover :: Int
numberOfRowsInExactCover = size ^ 3

numberOfColumnsInExactCover :: Int
numberOfColumnsInExactCover= size ^ 2 * 4
--}

cellSize :: Int
cellSize = 3

size :: Int
size = cellSize ^ 2

values :: [Int]
values = [1..size]


-- A value, row, column triple (x, y, z)
type CellCoordinate = (Int,Int,Int)

-- Generate all cell coordinates 
allCellCoordinates :: [CellCoordinate]
allCellCoordinates = [(v,r,c) | c <- values,r <- values,v <- values]

toExactCover :: [(CellCoordinate,[Int])]
toExactCover= [(r, toExactCoverColumns r) | r <- allCellCoordinates]

-- Each cell coordinate covers fullfils  four constraints.
toExactCoverColumns :: CellCoordinate -> [Int]
toExactCoverColumns (v,r,c) = [firstConstraint, secondConstraint, thirdConstraint, fourthConstraint] where firstConstraint = c + ((r-1) * size)
                                                                                                           secondConstraint = size^2 + v + ((r-1) * size)
                                                                                                           thirdConstraint = (size^2) * 2 + v + ((c-1)* size)
                                                                                                           fourthConstraint = (size^2) * 3 + ((region c r - 1) * size) + v

-- Calculate the 1 based region number, from top left in left to right fashion
region :: Int -> Int -> Int
region c r = 1 + boxRow * cellSize + boxColumn where boxRow= div (r-1) cellSize
                                                     boxColumn = div (c-1) cellSize

-- Tranform the traditional representation to the one expected by our DLX implementation
transform :: [(CellCoordinate, [Int])] -> Matrix Int CellCoordinate
transform x = zip columns rows where columns = sort $ nub $ concatMap snd x
                                     rows = map (columnToRow  x) columns

columnToRow ::[(CellCoordinate, [Int])] -> Int -> [CellCoordinate]
columnToRow x i = [coordinate | (coordinate,columns)<- x, i `elem` columns]
        
coverInitialPositions :: (Ord b, Ord a) => Matrix a b -> [b] -> Matrix a b
coverInitialPositions m [] = m
coverInitialPositions m (r:rs) = coverInitialPositions m' rs where m'=cover m r

solveSudoku :: [CellCoordinate] -> [CellCoordinate]
solveSudoku ex = solved where constraints = transform toExactCover
                              covered = coverInitialPositions constraints ex
                              solved =  solve' covered ex

