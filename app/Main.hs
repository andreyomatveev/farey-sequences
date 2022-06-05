module Main where

import FareySequencesModule

main :: IO ()
main = do
  putStrLn ("\n" ++ "predecessorInFm 6 (2 % 3)   returns   " ++ show (predecessorInFm 6 (2 % 3)) ++ "\n")
  putStrLn ("successorInFm 6 (1 % 3)   returns   " ++ show (successorInFm 6 (1 % 3)) ++ "\n")
  putStrLn ("predecessorInFml 6 4 (1 % 1)   returns   " ++ show (predecessorInFml 6 4 (1 % 1)) ++ "\n")
  putStrLn ("successorInFml 6 4 (4 % 5)   returns   " ++ show (successorInFml 6 4 (4 % 5)) ++ "\n")
  putStrLn ("predecessorInGml 6 4 (1 % 3)   returns   " ++ show (predecessorInGml 6 4 (1 % 3)) ++ "\n")
  putStrLn ("successorInGml 6 4 (1 % 3)   returns   " ++ show (successorInGml 6 4 (1 % 3)) ++ "\n")
  putStrLn ("predecessorInFBnm 6 4 (3 % 4)   returns   " ++ show (predecessorInFBnm 6 4 (3 % 4)) ++ "\n")
  putStrLn ("successorInFBnm 6 4 (4 % 5)   returns   " ++ show (successorInFBnm 6 4 (4 % 5)) ++ "\n")
  putStrLn ("predecessorOfPairOfNeighborsInFm 6 (1 % 3,  2 % 5) True   returns   " ++ show (predecessorOfPairOfNeighborsInFm 6 (1 % 3, 2 % 5) True) ++ "\n")
  putStrLn ("predecessorOfPairOfNeighborsInFm 6 (predecessorInFm 6 (2 % 5), 2 % 5) False   returns   " ++ show (predecessorOfPairOfNeighborsInFm 6 (predecessorInFm 6 (2 % 5), 2 % 5) False) ++ "\n")
  putStrLn ("successorOfPairOfNeighborsInFm 6 (3 % 5, 2 % 3) True   returns   " ++ show (successorOfPairOfNeighborsInFm 6 (3 % 5, 2 % 3) True) ++ "\n")
  putStrLn ("successorOfPairOfNeighborsInFm 6 (3 % 5, successorInFm 6 (3 % 5)) False   returns   " ++ show (successorOfPairOfNeighborsInFm 6 (3 % 5, successorInFm 6 (3 % 5)) False) ++ "\n")
  putStrLn ("predecessorOfPairOfNeighborsInFml 6 4 (4 % 5, 1 % 1) True   returns   " ++ show (predecessorOfPairOfNeighborsInFml 6 4 (4 % 5, 1 % 1) True) ++ "\n")
  putStrLn ("predecessorOfPairOfNeighborsInFml 6 4 (predecessorInFml 6 4 (1 % 1), 1 % 1) False   returns   " ++ show (predecessorOfPairOfNeighborsInFml 6 4 (predecessorInFml 6 4 (1 % 1), 1 % 1) False) ++ "\n")
  putStrLn ("successorOfPairOfNeighborsInFml 6 4 (3 % 4, 4 % 5) True   returns   " ++ show (successorOfPairOfNeighborsInFml 6 4 (3 % 4, 4 % 5) True) ++ "\n")
  putStrLn ("successorOfPairOfNeighborsInFml 6 4 (3 % 4, successorInFml 6 4 (3 % 4)) False   returns   " ++ show (successorOfPairOfNeighborsInFml 6 4 (3 % 4, successorInFml 6 4 (3 % 4)) False) ++ "\n")
  putStrLn ("predecessorOfPairOfNeighborsInGml 6 4 (1 % 2, 3 % 5) True   returns   " ++ show (predecessorOfPairOfNeighborsInGml 6 4 (1 % 2, 3 % 5) True) ++ "\n")
  putStrLn ("predecessorOfPairOfNeighborsInGml 6 4 (predecessorInGml 6 4 (3 % 5), 3 % 5) False   returns   " ++ show (predecessorOfPairOfNeighborsInGml 6 4 (predecessorInGml 6 4 (3 % 5), 3 % 5) False) ++ "\n")
  putStrLn ("successorOfPairOfNeighborsInGml 6 4 (1 % 3, 1 % 2) True   returns   " ++ show (successorOfPairOfNeighborsInGml 6 4 (1 % 3, 1 % 2) True) ++ "\n")
  putStrLn ("successorOfPairOfNeighborsInGml 6 4 (1 % 3, successorInGml 6 4 (1 % 3)) False   returns   " ++ show (successorOfPairOfNeighborsInGml 6 4 (1 % 3, successorInGml 6 4 (1 % 3)) False) ++ "\n")
  putStrLn ("predecessorOfPairOfNeighborsInFBnm 6 4 (4 % 5, 1 % 1) True   returns   " ++ show (predecessorOfPairOfNeighborsInFBnm 6 4 (4 % 5, 1 % 1) True) ++ "\n")
  putStrLn ("predecessorOfPairOfNeighborsInFBnm 6 4 (predecessorInFBnm 6 4 (1 % 1), 1 % 1) False   returns   " ++ show (predecessorOfPairOfNeighborsInFBnm 6 4 (predecessorInFBnm 6 4 (1 % 1), 1 % 1) False) ++ "\n")
  putStrLn ("successorOfPairOfNeighborsInFBnm 6 4 (1 % 3, 1 % 2) True   returns   " ++ show (successorOfPairOfNeighborsInFBnm 6 4 (1 % 3, 1 % 2) True) ++ "\n")
  putStrLn ("successorOfPairOfNeighborsInFBnm 6 4 (1 % 3, successorInFBnm 6 4 (1 % 3)) False   returns   " ++ show (successorOfPairOfNeighborsInFBnm 6 4 (1 % 3, successorInFBnm 6 4 (1 % 3)) False) ++ "\n")