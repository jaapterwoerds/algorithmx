module Matrix(Matrix,
              getRows, 
              getCols, 
              deleteCols, 
              deleteRows,
              occurences, 
              getColumnsSorted) where
import Data.List

{-- A matrix representation used to solve the exact cover problem. The matrix is a list of (column, row pairs). The columns represent the 
elements of the set X to be covered. The rows represent S, the subsets of X.
--}
type Matrix columntype rowtype= [(columntype,[rowtype])]

-- Get the list of columns in this matrix
getColumns :: Matrix c r -> [c]
getColumns = map fst

-- Get the rows for which which contain the given column
getRows :: Ord c => Ord r =>Matrix c r-> c -> [r] 
getRows matrix column = (snd . head) [(c,r) | (c,r)<-matrix , c==column]

-- Get the columns which are contained in the given row
getCols :: Ord c => Ord r =>Matrix c r-> r -> [c] 
getCols matrix row = map fst [(c,r) | (c,r) <- matrix, row `elem` r]

-- Remove a list of columns from the matrix
deleteCols :: Eq c => [c] -> [(c, r)] -> [(c, r)]
deleteCols columns matrix= [(c,r) | (c,r) <- matrix, c `notElem` columns]

-- Remove a list of rows from the matrix
deleteRows :: Eq r => [r] -> [(c, [r])] -> [(c, [r])]
deleteRows rows matrix= [(c, r \\ rows) | (c, r) <- matrix]

-- Get the number of occurences of the given column in the rows of the matrix
occurences ::Ord c => Ord r => Matrix c r -> c -> Int
occurences m columnIndex = length column where column = (snd . head) columns
                                               columns = filter (\p -> fst p==columnIndex) m
                                               
-- Get the columns sorted by the numer of their occurences in the rows of the matrix
getColumnsSorted::Ord c => Ord r => Matrix c r -> [c]
getColumnsSorted m = map fst sortedColumnToCount where counts= map (length . snd) m
                                                       columns=getColumns m
                                                       columnToCount = zip columns counts
                                                       sortedColumnToCount= sortBy least columnToCount

least ::Ord c => Ord r => (c,r) -> (c,r) -> Ordering
least (x,a) (y,b) | a < b = LT
                  | a > b= GT 
                  | a==b && x<y = LT
                  | a==b && x>y = GT
                  | otherwise = EQ