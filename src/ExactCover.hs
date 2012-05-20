module ExactCover (Matrix, solve, solve', cover) where
import Matrix
import Data.List
{-- 
Implementation of Donald Knuth's algorithm X in haskell. Algorithm X is a well known algorithm to solve the exact cover problem.
Given a set X and a S, a collection of subsets of X, determine the collection of subsets S* of S that cover each element of X exactly once.

For a more thorough explanation see http://en.wikipedia.org/wiki/Exact_cover
For a similar approach in Python see: http://www.cs.mcgill.ca/~aassaf9/python/algorithm_x.html
--}

-- Select the column from the matrix which has the lowest count of occurences in the rows of the matrix 
selectColumn :: Ord c => Ord r => Matrix c r -> c
selectColumn m = head $ getColumnsSorted m

-- Solve the exact cover problem as encoded in the given matrix
solve :: Ord c => Ord r => Matrix c r -> [r]
solve matrix = solve' matrix []

-- The recursive function that does the actual heavy lifting of solving the exact cover problem.
solve' :: Ord c => Ord r => Matrix c r->[r] -> [r]
solve' [] solution = solution                                -- The base case, the reduced matrix is empty.
solve' m solution = [s |  let c = selectColumn m,            -- Otherwise, select a column to be covered deterministically
                          occurences m c > 0,                -- If the element represented by column c is not contained in any row, terminate unsuccessfully, 
                          r <- getRows m c,                  -- nondeterministically choose a row that covers the element represented by column c 
                          let solution' = solution ++ [r],   -- Include row r in the partial solution.
                          let m' = cover m r,
                          s <- solve' m' solution']          -- Repeat this algorithm recursively on the reduced matrix m'

-- Cover the matrix with the given row. Remove the columns contained in that row and remove all other rows that contain at least one element also contained in the given row.
cover :: Ord c => Ord r => Matrix c r -> r -> Matrix c r
cover m r = deleteCols (columnsToDelete r)  $ deleteRows (rowsToDelete r) m where columnsToDelete = getCols m
                                                                                  rowsToDelete row = (nub . concat) $ map (getRows m) (getCols m row)
