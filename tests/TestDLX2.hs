{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import ExactCover

import System.Environment(getArgs)
import Test.Framework

emptyMatrix :: Matrix Int Char
emptyMatrix = []

--The empty set, base case
test_emptyMatrixNoResult = assertEqual [] (solve emptyMatrix)

-- A simple case of a singleton set to be covered
oneColumn::Matrix Int Char
oneColumn = [(1, "A")]
test_MatrixWithOneColumn = assertEqual "A" (solve oneColumn)

-- A simple exact cover example
simpleMatrix::Matrix Int Char
simpleMatrix = [
    (1, "AB"),
    (2, "EF"),
    (3, "DE"),
    (4, "ABC"),
    (5, "CD"),
    (6, "DE"),
    (7, "ACEF")]
test_simpleMatrix = assertEqual "BDF" (solve simpleMatrix)    

-- A case in which there is no exact cover possible
matrixWithoutSolution = [(1, "A"),(2,[])]
test_matrixNoSolution = assertEqual [] (solve matrixWithoutSolution) 

-- Run the tests
main =
    do args <- getArgs
       runTestWithArgs args allHTFTests