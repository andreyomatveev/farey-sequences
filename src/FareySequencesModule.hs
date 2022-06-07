-- Andrey O. Matveev
-- After several selected formulas from the monograph A.O. Matveev, Farey Sequences: Duality and Maps Between Subsequences,
-- Berlin: De Gruyter, 2017, https://doi.org/10.1515/9783110547665 .
--
-- Our Dramatis Personae (see Table 1.1 of the monograph):
--
-- (Standard) Farey Sequence Fm ( in LaTeX: $\mathcal{F}_m$ ), where m > 0;
-- Farey Subsequence Fml ( in LaTeX: $\mathcal{F}_m^l$ ), where m > 1, and 0 < l < m;
-- Farey Subsequence Gml ( in LaTeX: $\mathcal{G}_m^l$ ), where m > 1, and 0 < l < m;
-- Farey Subsequence FB2mm ( in LaTeX: $\mathcal{F}(\mathbb{B}(2m),m)$ ), where m > 0;
-- Farey Subsequence FBnm ( in LaTeX: $\mathcal{F}(\mathbb{B}(n),m)$ ), where n > 1, and 0 < m < n.
--
-- In healthy situations, all of the exported functions below return NONNEGATIVE reduced fractions (h % k) such that (0 % 1) <= (h % k) <= (1 % 1).
-- If you get a NEGATIVE fraction, it means that something went wrong,
-- and the denominator of the resulting negative fraction has no computational meaning,
-- since it just reports a reason of the problem---see the source code of the function you have used.
--
-- If you would like to build up a certain subsequence of neighboring (one after another) fractions
-- in one of the above Personae, then first you should realize what you hold in your hands in the beginning.
--
-- If you have two neighboring fractions, then you proceed in the recurrent manner
-- by means of a relatively fast function of the form `predecessorOfPairOfNeighborsInPersonage' or
-- `successorOfPairOfNeighborsInPersonage', depending on which direction (descending or ascending) you take.
--
-- If you have one fraction then, in order to obtain your starting pair of neighboring fractions,
-- you make one-time use of a relatively slow function of the form `predecessorInPersonage' or `successorInPersonage',
-- and then you proceed by calling, in the recurrent manner, the corresponding relatively fast function
-- of the form  `predecessorOfPairOfNeighborsInPersonage' or `successorOfPairOfNeighborsInPersonage'.
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module FareySequencesModule
  ( module Data.List,
    module Data.Maybe,
    module GHC.Real,
    predecessorInFm,
    successorInFm,
    predecessorInFml,
    successorInFml,
    predecessorInGml,
    successorInGml,
    predecessorInFBnm,
    successorInFBnm,
    predecessorOfPairOfNeighborsInFm,
    successorOfPairOfNeighborsInFm,
    predecessorOfPairOfNeighborsInFml,
    successorOfPairOfNeighborsInFml,
    predecessorOfPairOfNeighborsInGml,
    successorOfPairOfNeighborsInGml,
    predecessorOfPairOfNeighborsInFBnm,
    successorOfPairOfNeighborsInFBnm,
    -- In theory, Farey Subsequences $\mathcal{F}(\mathbb{B}(n),m)$ exhibit different behavior
    -- depending on whether you have n == 2*m, or n /= 2*m. Nevertheless, if you deal with a Farey Subsequence $\mathcal{F}(\mathbb{B}(2m),m)$,
    -- then in this module it is suffices to call the predecessorInFBnm, successorInFBnm, predecessorOfPairOfNeighborsInFBnm,
    -- and successorOfPairOfNeighborsInFBnm functions by giving the value of (2 * m) for the parameter n.
    --
    -- If, for some reason, you prefer more narrowed names of functions, uncomment (and import and use the corresponding functions)
    -- the following lines:
    {-
    predecessorInFB2mm,
    successorInFB2mm,
    predecessorOfPairOfNeighborsInFB2mm,
    successorOfPairOfNeighborsInFB2mm,
    -}
  )
where

import Data.List (find)
import Data.Maybe (fromMaybe)
import GHC.Real (denominator, numerator, (%))

predecessorInFm :: Integer -> Rational -> Rational
-- See Lemma 2.9(i) and Table 2.1 of the monograph. Call for instance:
-- ghci> predecessorInFm 6 (2 % 3)
-- to get the result:
-- 3 % 5
predecessorInFm m successor
  | m < 1 = -1 % 1 -- "N/A: Order m of the sequence should be > 0"
  | successor <= 0 % 1 || successor > 1 % 1 = -1 % 2 --"N/A: Successor should be between (0 % 1) (excluded) and (1 % 1) (included)"
  | denominator successor > m = -1 % 3 --"N/A: Denominator of the successor should not exceed the order m of the sequence"
  | otherwise =
    if successor == 1 % 1
      then predecessorOfOneFirstInFm m
      else
        let refPoint = ceiling (toRational (numerator successor * m) / toRational (denominator successor))
         in getNumeratorAndReturnPredecesssor (findNumeratorOfPredecessor (refPoint - numerator successor, refPoint - 1) successor) successor

successorInFm :: Integer -> Rational -> Rational
-- See Lemma 2.9(ii) and Table 2.3 of the monograph. Call for instance:
-- ghci> successorInFm 6 (1 % 3)
-- to get the result:
-- 2 % 5
successorInFm m predecessor
  | m < 1 = -1 % 1 -- "N/A: Order m of the sequence should be > 0"
  | predecessor < 0 % 1 || predecessor >= 1 % 1 = -1 % 2 --"N/A: Predecessor should be between 0%1 (included) and 1%1 (excluded)"
  | denominator predecessor > m = -1 % 3 --"N/A: Denominator of the predecessor should not exceed the order m of the sequence"
  | otherwise =
    if predecessor == 0 % 1
      then successorOfZeroFirstInFm m
      else
        let refPoint = ceiling (toRational (numerator predecessor * m + 2) / toRational (denominator predecessor))
         in getNumeratorAndReturnSuccessor (findNumeratorOfSuccessor (refPoint - numerator predecessor, refPoint - 1) predecessor) predecessor

predecessorInFml :: Integer -> Integer -> Rational -> Rational
-- See Lemma 2.13(i)(a)-(b) and Table 2.1 of the monograph. Call for instance:
-- ghci> predecessorInFml 6 4 (1 % 1)
-- to get the result:
-- 4 % 5
predecessorInFml m l successor
  | m < 2 = -1 % 1 -- "N/A: Parameter m of the sequence should be > 1"
  | l <= 0 || l >= m = -1 % 2 --"N/A: Parameter l should be between 0 (excluded) and m (excluded)"
  | successor <= 0 % 1 || successor > 1 % 1 = -1 % 3 --"N/A: Successor should be between (0 % 1) (excluded) and (1 % 1) (included)"
  | denominator successor > m = -1 % 4 --"N/A: Denominator of the successor should not exceed the parameter m of the sequence"
  | l < numerator successor = -1 % 5 --"N/A: Numerator of the successor should be between 1 (included) and l (included)"
  | otherwise =
    if successor == 1 % 1
      then predecessorOfOneFirstInFml m l
      else
        if numerator successor * m - denominator successor * l >= 1
          then getNumeratorAndReturnPredecesssor (findNumeratorOfPredecessor (l - numerator successor + 1, l) successor) successor
          else
            let refPoint = ceiling (toRational (numerator successor * m) / toRational (denominator successor))
             in getNumeratorAndReturnPredecesssor (findNumeratorOfPredecessor (refPoint - numerator successor, refPoint - 1) successor) successor

successorInFml :: Integer -> Integer -> Rational -> Rational
-- See Lemma 2.13(ii)(a)-(b) and Table 2.3 of the monograph. Call for instance:
-- ghci> successorInFml 6 4 (4 % 5)
-- to get the result:
-- 1 % 1
successorInFml m l predecessor
  | m < 2 = -1 % 1 -- "N/A: Parameter m of the sequence should be > 1"
  | l <= 0 || l >= m = -1 % 2 --"N/A: Parameter l should be between 0 (excluded) and m (excluded)"
  | predecessor < 0 % 1 || predecessor >= 1 % 1 = -1 % 3 --"N/A: Predecessor should be between 0%1 (included) and 1%1 (excluded)"
  | denominator predecessor > m = -1 % 4 --"N/A: Denominator of the predecessor should not exceed the parameter m of the sequence"
  | l < numerator predecessor = -1 % 5 --"N/A: Numerator of the predecessor should be between 1 (included) and l (included)"
  | otherwise =
    if predecessor == 0 % 1
      then successorOfZeroFirstInFml m l
      else
        if denominator predecessor * l - numerator predecessor * m >= 1
          then
            let refPoint = ceiling (toRational (numerator predecessor * m + 2) / toRational (denominator predecessor))
             in getNumeratorAndReturnSuccessor (findNumeratorOfSuccessor (refPoint - numerator predecessor, refPoint - 1) predecessor) predecessor
          else getNumeratorAndReturnSuccessor (findNumeratorOfSuccessor (l - numerator predecessor + 1, l) predecessor) predecessor

predecessorInGml :: Integer -> Integer -> Rational -> Rational
-- See Lemma 2.15(i)(a)-(b) and Table 2.1 of the monograph. Call for instance:
-- ghci> predecessorInGml 6 4 (1 % 3)
-- to get the result:
-- 0 % 1
predecessorInGml m l successor
  | m < 2 = -1 % 1 -- "N/A: Parameter m of the sequence should be > 1"
  | l <= 0 || l >= m = -1 % 2 --"N/A: Parameter l should be between 0 (excluded) and m (excluded)"
  | successor <= 0 % 1 || successor > 1 % 1 = -1 % 3 --"N/A: Successor should be between (0 % 1) (excluded) and (1 % 1) (included)"
  | denominator successor > m = -1 % 4 --"N/A: Denominator of the successor should not exceed the parameter m of the sequence"
  | l + denominator successor - m > numerator successor = -1 % 5 --"N/A: The quantity (l + denominator - m) should not exceed the numerator of the successor"
  | otherwise =
    if successor == 1 % 1
      then predecessorOfOneFirstInGml m l
      else
        if numerator successor * m - denominator successor * l >= 1
          then
            let refPoint = ceiling (toRational (numerator successor * m) / toRational (denominator successor))
             in getNumeratorAndReturnPredecesssor (findNumeratorOfPredecessor (refPoint - numerator successor, refPoint - 1) successor) successor
          else
            let refPoint = ceiling (toRational (numerator successor * (m - l)) / toRational (denominator successor - numerator successor))
             in getNumeratorAndReturnPredecesssor (findNumeratorOfPredecessor (refPoint - numerator successor, refPoint - 1) successor) successor

successorInGml :: Integer -> Integer -> Rational -> Rational
-- See Lemma 2.15(ii)(a)-(b) and Table 2.3 of the monograph. Call for instance:
-- ghci> successorInGml 6 4 (1 % 3)
-- to get the result:
-- 1 % 2
successorInGml m l predecessor
  | m < 2 = -1 % 1 -- "N/A: Parameter m of the sequence should be > 1"
  | l <= 0 || l >= m = -1 % 2 --"N/A: Parameter l should be between 0 (excluded) and m (excluded)"
  | predecessor < 0 % 1 || predecessor >= 1 % 1 = -1 % 3 --"N/A: Predecessor should be between (0 % 1) (included) and (1 % 1) (excluded)"
  | denominator predecessor > m = -1 % 4 --"N/A: Denominator of the predecessor should not exceed the parameter m of the sequence"
  | l + denominator predecessor - m > numerator predecessor = -1 % 5 --"N/A: Denominator of the predecessor minus its numerator should not exceed (m - l)"
  | otherwise =
    if predecessor == 0 % 1
      then successorOfZeroFirstInGml m l
      else
        if denominator predecessor * l - numerator predecessor * m >= 1
          then
            let refPoint = ceiling (toRational (numerator predecessor * (m - l) + 2) / toRational (denominator predecessor - numerator predecessor))
             in getNumeratorAndReturnSuccessor (findNumeratorOfSuccessor (refPoint - numerator predecessor, refPoint - 1) predecessor) predecessor
          else
            let refPoint = ceiling (toRational (numerator predecessor * m + 2) / toRational (denominator predecessor))
             in getNumeratorAndReturnSuccessor (findNumeratorOfSuccessor (refPoint - numerator predecessor, refPoint - 1) predecessor) predecessor

predecessorInFB2mm :: Integer -> Rational -> Rational
-- See Renark 1.17 and Table 1.5;
-- see Renark 2.25 and Table 2.8;
-- see Renark 2.11 and Table 2.5;
-- see Renark 2.24 and Table 2.7;
-- see Proposition 2.12 (i) (a) and Proposition 2.12 (ii) (a), and Table 2.1 of the monograph. Call for instance:
-- ghci> predecessorInFB2mm 3 (2 % 5)
-- to get the result:
-- 1 % 3
predecessorInFB2mm m successor
  | m < 1 = -1 % 1 -- "N/A: Parameter m of the sequence should be > 0"
  | successor <= 0 % 1 || successor > 1 % 1 = -1 % 2 --"N/A: Successor should be between (0 % 1) (excluded) and (1 % 1) (included)"
  | denominator successor > 2 * m = -1 % 3 --"N/A: Denominator of the successor should not exceed  (2 * m)"
  | denominator successor - m > numerator successor || numerator successor > m = -1 % 4 --"N/A: Numerator of the successor should be between (denominator - m) (included) and m (included)"
  | otherwise =
    if
        | successor == 1 % 1 -> predecessorOfOneFirstInFB2mm m
        | successor == 2 % 3 -> predecessorOfTwoThirdsInFB2mm m
        | successor == 1 % 2 -> predecessorOfOneSecondInFB2mm m
        | successor == 1 % 3 -> predecessorOfOneThirdInFB2mm m
        | otherwise ->
          if successor > 1 % 2
            then getNumeratorAndReturnPredecesssor (findNumeratorOfPredecessor (m - numerator successor + 1, m) successor) successor
            else
              let refPoint = ceiling (toRational (numerator successor * m) / toRational (denominator successor - numerator successor))
               in getNumeratorAndReturnPredecesssor (findNumeratorOfPredecessor (refPoint - numerator successor, refPoint - 1) successor) successor

successorInFB2mm :: Integer -> Rational -> Rational
-- See Renark 1.17 and Table 1.5;
-- see Renark 2.24 and Table 2.7;
-- see Renark 2.11 and Table 2.5;
-- see Renark 2.25 and Table 2.8;
-- see Proposition 2.12 (i) (b) and Proposition 2.12 (ii) (b), and Table 2.3 of the monograph. Call for instance:
-- ghci> successorInFB2mm 3 (3 % 5)
-- to get the result:
-- 2 % 3
successorInFB2mm m predecessor
  | m < 1 = -1 % 1 -- "N/A: Parameter m of the sequence should be > 0"
  | predecessor < 0 % 1 || predecessor >= 1 % 1 = -1 % 2 --"N/A: Predecessor should be between (0 % 1) (excluded) and (1 % 1) (included)"
  | denominator predecessor > 2 * m = -1 % 3 --"N/A: Denominator of the predecessor should not exceed (2 * m)"
  | denominator predecessor - m > numerator predecessor || numerator predecessor > m = -1 % 4 --"N/A: Numerator of the predecessor should be between (denominator - m) (included) and m (included)"
  | otherwise =
    if
        | predecessor == 0 % 1 -> successorOfZeroFirstInFB2mm m
        | predecessor == 1 % 3 -> successorOfOneThirdInFB2mm m
        | predecessor == 1 % 2 -> successorOfOneSecondInFB2mm m
        | predecessor == 2 % 3 -> successorOfTwoThirdsInFB2mm m
        | otherwise ->
          if predecessor < 1 % 2
            then
              let refPoint = ceiling (toRational (numerator predecessor * m + 2) / toRational (denominator predecessor - numerator predecessor))
               in getNumeratorAndReturnSuccessor (findNumeratorOfSuccessor (refPoint - numerator predecessor, refPoint - 1) predecessor) predecessor
            else getNumeratorAndReturnSuccessor (findNumeratorOfSuccessor (m - numerator predecessor + 1, m) predecessor) predecessor

predecessorInFBnm :: Integer -> Integer -> Rational -> Rational
-- See Remark 1.17 and Table 1.5 of the monograph;
-- see CORRECTED Remark 2.43(i) and Remark 2.43(ii) and CORRECTED Table 2.8;
-- see Remark 2.17 and Table 2.5;
-- See CORRECTED Remark 2.42 and Table 2.7;
-- see Propositions 2.18(i)(a) and 2.18(ii)(a), and Table 2.1;
-- see Propositions 2.19(i)(a) and 2.19(ii)(a), and Table 2.1.  Call for instance:
-- ghci> predecessorInFBnm 6 4 (3 % 4)
-- to get the result:
-- 2 % 3
predecessorInFBnm n m successor
  | n == 2 * m = predecessorInFB2mm m successor
  | n < 2 = -1 % 1 -- "N/A: Parameter n of the sequence should be > 1"
  | m < 1 || m >= n = -1 % 2 -- "N/A: Parameter m of the sequence should be between 0 (excluded) and n (excluded)"
  | successor <= 0 % 1 || successor > 1 % 1 = -1 % 3 --"N/A: Successor should be between (0 % 1) (excluded) and (1 % 1) (included)"
  | denominator successor > n = -1 % 4 --"N/A: Denominator of the successor should not exceed n"
  | m + denominator successor - n > numerator successor || numerator successor > m = -1 % 5 --"N/A: Numerator of the successor should be between (m + denominator - n) (included) and m (included)"
  | otherwise =
    if
        | successor == 1 % 1 -> predecessorOfOneFirstInFBnm n m
        | successor == 2 % 3 -> predecessorOfTwoThirdsInFBnm n m
        | successor == 1 % 2 -> predecessorOfOneSecondInFBnm n m
        | successor == 1 % 3 -> predecessorOfOneThirdInFBnm n m
        | otherwise ->
          if n < 2 * m
            then
              ( if (successor > 1 % 2) && (numerator successor * n - denominator successor * m >= 1)
                  then getNumeratorAndReturnPredecesssor (findNumeratorOfPredecessor (m - numerator successor + 1, m) successor) successor
                  else
                    ( let refPoint = ceiling (toRational (numerator successor * (n - m)) / toRational (denominator successor - numerator successor))
                       in getNumeratorAndReturnPredecesssor (findNumeratorOfPredecessor (refPoint - numerator successor, refPoint - 1) successor) successor
                    )
              )
            else -- n > 2 * m

              ( if (successor > 1 % 2) || ((numerator successor * n - denominator successor * m) >= 1)
                  then getNumeratorAndReturnPredecesssor (findNumeratorOfPredecessor (m - numerator successor + 1, m) successor) successor
                  else
                    ( let refPoint = ceiling (toRational (numerator successor * (n - m)) / toRational (denominator successor - numerator successor))
                       in getNumeratorAndReturnPredecesssor (findNumeratorOfPredecessor (refPoint - numerator successor, refPoint - 1) successor) successor
                    )
              )

successorInFBnm :: Integer -> Integer -> Rational -> Rational
-- See Remark 1.17 and Table 1.5 of the monograph;
-- see CORRECTED Remark 2.42 and Table 2.7;
-- see Remark 2.17 and Table 2.5;
-- see Remark 2.43(i)-(ii) and Table 2.8;
-- see Propositions 2.18(i)(b) and 2.18(ii)(b), and Table 2.3;
-- see Propositions 2.19(i)(b) and 2.19(ii)(b), and Table 2.3.  Call for instance:
-- ghci> successorInFBnm 6 4 (4 % 5)
-- to get the result:
-- 1 % 1
successorInFBnm n m predecessor
  | n == 2 * m = successorInFB2mm m predecessor
  | n < 2 = -1 % 1 -- "N/A: Parameter n of the sequence should be > 1"
  | m < 1 || m >= n = -1 % 2 -- "N/A: Parameter m of the sequence should be between 0 (excluded) and n (excluded)"
  | predecessor < 0 % 1 || predecessor >= 1 % 1 = -1 % 3 --"N/A: Predecessor should be between (0 % 1) (excluded) and (1 % 1) (included)"
  | denominator predecessor > n = -1 % 4 --"N/A: Denominator of the predecessor should not exceed `n' "
  | m + denominator predecessor - n > numerator predecessor || numerator predecessor > m = -1 % 5 --"N/A: Numerator of the predecessor should be between (m + denominator - n) (included) and m (included)"
  | otherwise =
    if
        | predecessor == 0 % 1 -> successorOfZeroFirstInFBnm n m
        | predecessor == 1 % 3 -> successorOfOneThirdInFBnm n m
        | predecessor == 1 % 2 -> successorOfOneSecondInFBnm n m
        | predecessor == 2 % 3 -> successorOfTwoThirdsInFBnm n m
        | otherwise ->
          if n < 2 * m
            then
              ( if (predecessor > 1 % 2) && (denominator predecessor * m - numerator predecessor * n <= 1)
                  then getNumeratorAndReturnSuccessor (findNumeratorOfSuccessor (m - numerator predecessor + 1, m) predecessor) predecessor
                  else
                    ( let refPoint = ceiling (toRational (numerator predecessor * (n - m) + 2) / toRational (denominator predecessor - numerator predecessor))
                       in getNumeratorAndReturnSuccessor (findNumeratorOfSuccessor (refPoint - numerator predecessor, refPoint - 1) predecessor) predecessor
                    )
              )
            else -- n > 2*m

              ( if (predecessor > 1 % 2) || (denominator predecessor * m - numerator predecessor * n <= 1)
                  then getNumeratorAndReturnSuccessor (findNumeratorOfSuccessor (m - numerator predecessor + 1, m) predecessor) predecessor
                  else
                    ( let refPoint = ceiling (toRational (numerator predecessor * (n - m) + 2) / toRational (denominator predecessor - numerator predecessor))
                       in getNumeratorAndReturnSuccessor (findNumeratorOfSuccessor (refPoint - numerator predecessor, refPoint - 1) predecessor) predecessor
                    )
              )

getNumeratorAndReturnPredecesssor :: Integer -> Rational -> Rational
getNumeratorAndReturnPredecesssor a successor = a % ((denominator successor * a + 1) `div` numerator successor)

getNumeratorAndReturnSuccessor :: Integer -> Rational -> Rational
getNumeratorAndReturnSuccessor a predecessor = a % ((denominator predecessor * a - 1) `div` numerator predecessor)

findNumeratorOfPredecessor :: (Integer, Integer) -> Rational -> Integer
findNumeratorOfPredecessor searchInterval successor =
  fromMaybe 0 (find (\x -> (denominator successor * x + 1) `mod` numerator successor == 0) [fst searchInterval .. snd searchInterval])

findNumeratorOfSuccessor :: (Integer, Integer) -> Rational -> Integer
findNumeratorOfSuccessor searchInterval predecessor =
  fromMaybe 0 (find (\x -> (denominator predecessor * x - 1) `mod` numerator predecessor == 0) [fst searchInterval .. snd searchInterval])

predecessorOfOneFirstInFm :: Integer -> Rational
-- See Remark 1.6 and Table 1.5  of the monograph
predecessorOfOneFirstInFm m = (m - 1) % m

successorOfZeroFirstInFm :: Integer -> Rational
-- See Remark 1.6 and Table 1.5  of the monograph
successorOfZeroFirstInFm m = 1 % m

predecessorOfOneFirstInFml :: Integer -> Integer -> Rational
-- See Remark 1.9 and Table 1.5  of the monograph
predecessorOfOneFirstInFml m l = l % (l + 1)

successorOfZeroFirstInFml :: Integer -> Integer -> Rational
-- See Remark 1.9 and Table 1.5  of the monograph
successorOfZeroFirstInFml m l = 1 % m

predecessorOfOneFirstInGml :: Integer -> Integer -> Rational
-- See Remark 1.13 and Table 1.5  of the monograph
predecessorOfOneFirstInGml m l = (m - 1) % m

successorOfZeroFirstInGml :: Integer -> Integer -> Rational
-- See Remark 1.13 and Table 1.5  of the monograph
successorOfZeroFirstInGml m l = 1 % (m - l + 1)

predecessorOfOneFirstInFB2mm :: Integer -> Rational
-- See Remark 1.17 and Table 1.5 of the monograph
predecessorOfOneFirstInFB2mm m = m % (m + 1)

predecessorOfTwoThirdsInFB2mm :: Integer -> Rational
-- See Remark 2.25 and Table 2.8 of the monograph
predecessorOfTwoThirdsInFB2mm m =
  if even m
    then (m - 1) % ((3 * m - 2) `div` 2)
    else m % ((3 * m + 1) `div` 2)

predecessorOfOneSecondInFB2mm :: Integer -> Rational
-- See Remark 2.11 and Table 2.5 of the monograph
predecessorOfOneSecondInFB2mm m = (m - 1) % (2 * m - 1)

predecessorOfOneThirdInFB2mm :: Integer -> Rational
-- See Remark 2.24 and Table 2.7 of the monograph
predecessorOfOneThirdInFB2mm m =
  if even m
    then (m - 2) `div` 2 % ((3 * m - 4) `div` 2)
    else (m - 1) `div` 2 % ((3 * m - 1) `div` 2)

successorOfZeroFirstInFB2mm :: Integer -> Rational
-- See Remark 1.17 and Table 1.5 of the monograph
successorOfZeroFirstInFB2mm m = 1 % (m + 1)

successorOfOneThirdInFB2mm :: Integer -> Rational
-- See Remark 2.24 and Table 2.7 of the monograph
successorOfOneThirdInFB2mm m =
  if even m
    then m `div` 2 % ((3 * m - 2) `div` 2)
    else (m + 1) `div` 2 % ((3 * m + 1) `div` 2)

successorOfOneSecondInFB2mm :: Integer -> Rational
-- See Remark 2.11 and Table 2.5 of the monograph
successorOfOneSecondInFB2mm m = m % (2 * m - 1)

successorOfTwoThirdsInFB2mm :: Integer -> Rational
-- See Remark 2.25 and Table 2.8 of the monograph
successorOfTwoThirdsInFB2mm m =
  if even m
    then (m - 1) % ((3 * m - 4) `div` 2)
    else m % ((3 * m - 1) `div` 2)

predecessorOfOneFirstInFBnm :: Integer -> Integer -> Rational
-- See Remark 1.17 and Table 1.5 of the monograph
predecessorOfOneFirstInFBnm n m = m % (m + 1)

predecessorOfTwoThirdsInFBnm :: Integer -> Integer -> Rational
-- See CORRECTED Remark 2.43(i) and Remark 2.43(ii) and CORRECTED Table 2.8 of the monograph
predecessorOfTwoThirdsInFBnm n m
  | n < 2 * m =
    if 2 * n - 3 * m >= 1
      then
        if even m
          then (m - 1) % ((3 * m - 2) `div` 2)
          else m % ((3 * m + 1) `div` 2)
      else (2 * (n - m) - 1) % (3 * (n - m) - 1)
  | even m = (m - 1) % ((3 * m - 2) `div` 2)
  | otherwise = m % ((3 * m + 1) `div` 2)

predecessorOfOneSecondInFBnm :: Integer -> Integer -> Rational
-- See Remark 2.17 and Table 2.5 of the monograph
predecessorOfOneSecondInFBnm n m
  | n < 2 * m = (n - m - 1) % (2 * (n - m) - 1)
  | otherwise = m % (2 * m + 1)

predecessorOfOneThirdInFBnm :: Integer -> Integer -> Rational
-- See CORRECTED Remark 2.42 and Table 2.7 of the monograph
predecessorOfOneThirdInFBnm n m
  | n < 2 * m =
    if even (n - m)
      then (n - m - 2) `div` 2 % ((3 * (n - m) - 4) `div` 2)
      else (n - m - 1) `div` 2 % ((3 * (n - m) - 1) `div` 2)
  | n - 3 * m >= 1 = m % (3 * m + 1)
  | even (n - m) = (n - m - 2) `div` 2 % ((3 * (n - m) - 4) `div` 2)
  | otherwise = (n - m - 1) `div` 2 % ((3 * (n - m) - 1) `div` 2)

successorOfZeroFirstInFBnm :: Integer -> Integer -> Rational
-- See Remark 1.17 and Table 1.5 of the monograph
successorOfZeroFirstInFBnm n m = 1 % (n - m + 1)

successorOfOneThirdInFBnm :: Integer -> Integer -> Rational
-- See CORRECTED Remark 2.42 and Table 2.7 of the monograph
successorOfOneThirdInFBnm n m
  | n < 2 * m =
    if even (n - m)
      then (n - m) `div` 2 % ((3 * (n - m) - 2) `div` 2)
      else (n - m + 1) `div` 2 % ((3 * (n - m) + 1) `div` 2)
  | 3 * m - n <= 1 = m % (3 * m - 1)
  | even (n - m) = (n - m) `div` 2 % ((3 * (n - m) - 2) `div` 2)
  | otherwise = (n - m + 1) `div` 2 % ((3 * (n - m) + 1) `div` 2)

successorOfOneSecondInFBnm :: Integer -> Integer -> Rational
-- See Remark 2.17 and Table 2.5 of the monograph
successorOfOneSecondInFBnm n m
  | n < 2 * m = (n - m + 1) % (2 * (n - m) + 1)
  | otherwise = m % (2 * m - 1)

successorOfTwoThirdsInFBnm :: Integer -> Integer -> Rational
-- See Remarks 2.43(i)-(ii) and Table 2.8 of the monograph
successorOfTwoThirdsInFBnm n m
  | n < 2 * m =
    if 3 * m - 2 * n >= 1
      then (2 * (n - m) + 1) % (3 * (n - m) + 1)
      else
        if even m
          then (m - 1) % ((3 * m - 4) `div` 2)
          else m % ((3 * m - 1) `div` 2)
  | even m = (m - 1) % ((3 * m - 4) `div` 2)
  | otherwise = m % ((3 * m - 1) `div` 2)

predecessorOfPairOfNeighborsInFm :: Integer -> (Rational, Rational) -> Bool -> Rational
-- See Proposition 1.25 and Table 1.6 of the monograph. Call for instance:
-- ghci> predecessorOfPairOfNeighborsInFm 6 (1 % 3,  2 % 5) True
-- to get the result:
-- 1 % 4
-- Also call:
-- ghci> predecessorOfPairOfNeighborsInFm 6 (predecessorInFm 6 (2 % 5), 2 % 5) False
-- to get the same result:
-- 1 % 4
predecessorOfPairOfNeighborsInFm m (successor, rightNeighborOfSuccessor) checkPair
  -- If checkPair == True, then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Fm
  | m < 1 = -1 % 1 -- "N/A: Order m of the sequence should be > 0"
  | successor >= rightNeighborOfSuccessor = -1 % 2 -- "N/A We should have successor < rightNeighborOfSuccessor"
  | successor <= 0 % 1 || successor >= 1 % 1 = -1 % 3 --"N/A: successor should be between (0 % 1) (excluded) and (1 % 1) (excluded)"
  | denominator successor > m = -1 % 4 --"N/A: Denominator of the successor should not exceed the order m of the sequence"
  | rightNeighborOfSuccessor > 1 % 1 = -1 % 5 --"N/A: rightNeighborOfSuccessor should be between successor (excluded) and (1 % 1) (included)"
  | denominator rightNeighborOfSuccessor > m = -1 % 6 --"N/A: Denominator of the rightNeighborOfSuccessor should not exceed the order m of the sequence"
  | not checkPair || (checkPair && (successor == predecessorInFm m rightNeighborOfSuccessor)) =
    let fareyIndex = floor (toRational (m + denominator rightNeighborOfSuccessor) / toRational (denominator successor))
     in (fareyIndex * numerator successor - numerator rightNeighborOfSuccessor) % (fareyIndex * denominator successor - denominator rightNeighborOfSuccessor)
  | otherwise = -1 % 7 -- "N/A: The input pair is not a pair of neighboring fractions in this Farey sequence"

successorOfPairOfNeighborsInFm :: Integer -> (Rational, Rational) -> Bool -> Rational
-- See Proposition 1.25 and Table 1.6 of the monograph. Call for instance:
-- ghci> successorOfPairOfNeighborsInFm 6 (3 % 5, 2 % 3) True
-- to get the result:
-- 3 % 4
-- Also call:
-- ghci>  successorOfPairOfNeighborsInFm 6 (3 % 5, successorInFm 6 (3 % 5)) False
-- to get the same result:
-- 3 % 4
successorOfPairOfNeighborsInFm m (leftNeighborOfPredecessor, predecessor) checkPair
  -- If checkPair == True, then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Fm
  | m < 1 = -1 % 1 -- "N/A: Order m of the sequence should be > 0"
  | leftNeighborOfPredecessor >= predecessor = -1 % 2 -- "N/A: We should have leftNeighborOfPredecessor < predecessor"
  | predecessor <= 0 % 1 || predecessor >= 1 % 1 = -1 % 3 --"N/A: predecessor should be between (0 % 1) (excluded) and (1 % 1) (excluded)"
  | denominator predecessor > m = -1 % 4 --"N/A: Denominator of the predecessor should not exceed the order m of the sequence"
  | leftNeighborOfPredecessor < 0 % 1 = -1 % 5 --"N/A: leftNeighborOfPredecessor should be between (0 % 1) (included) and predecessor (excluded)"
  | denominator leftNeighborOfPredecessor > m = -1 % 6 --"N/A: Denominator of the leftNeighborOfPredecessor should not exceed the order m of the sequence"
  | not checkPair || checkPair && predecessor == successorInFm m leftNeighborOfPredecessor =
    let fareyIndex = floor (toRational (m + denominator leftNeighborOfPredecessor) / toRational (denominator predecessor))
     in (fareyIndex * numerator predecessor - numerator leftNeighborOfPredecessor) % (fareyIndex * denominator predecessor - denominator leftNeighborOfPredecessor)
  | otherwise = -1 % 7 -- "N/A: The input pair is not a pair of neighboring fractions in this Farey sequence"

predecessorOfPairOfNeighborsInFml :: Integer -> Integer -> (Rational, Rational) -> Bool -> Rational
-- See Proposition 1.26 (ii) (a) and Table 1.6 of the monograph. Call for instance:
-- ghci> predecessorOfPairOfNeighborsInFml 6 4 (4 % 5, 1 % 1) True
-- to get the result:
-- 3 % 4
-- Also call:
-- ghci>  predecessorOfPairOfNeighborsInFml 6 4 (predecessorInFml 6 4 (1 % 1), 1 % 1) False
-- to get the same result:
-- 3 % 4
predecessorOfPairOfNeighborsInFml m l (successor, rightNeighborOfSuccessor) checkPair
  -- If checkPair == True, then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Fml
  | m < 2 = -1 % 1 -- "N/A: Parameter m of the sequence should be > 1"
  | l <= 0 || l >= m = -1 % 2 --"N/A: Parameter l should be between 0 (excluded) and m (excluded)"
  | successor >= rightNeighborOfSuccessor = -1 % 3 -- "N/A We should have successor < rightNeighborOfSuccessor"
  | successor <= 0 % 1 || successor > 1 % 1 = -1 % 4 --"N/A: successor should be between (0 % 1) (excluded) and (1 % 1) (included)"
  | denominator successor > m = -1 % 5 --"N/A: Denominator of the successor should not exceed the parameter m of the sequence"
  | l < numerator successor = -1 % 6 --"N/A: Numerator of the successor should be between 1 (included) and l (included)"
  | rightNeighborOfSuccessor > 1 % 1 = -1 % 7 --"N/A: rightNeighborOfSuccessor should be between successor (excluded) and (1 % 1) (included)"
  | denominator rightNeighborOfSuccessor > m = -1 % 8 --"N/A: Denominator of the rightNeighborOfSuccessor should not exceed the parameter m of the sequence"
  | l < numerator rightNeighborOfSuccessor = -1 % 9 --"N/A: Numerator of the rightNeighborOfSuccessor should be between 1 (included) and l (included)"
  | not checkPair || checkPair && successor == predecessorInFml m l rightNeighborOfSuccessor =
    if numerator successor * m - denominator successor * l >= 1
      then
        let fareyIndex = floor (toRational (l + numerator rightNeighborOfSuccessor) / toRational (numerator successor))
         in (fareyIndex * numerator successor - numerator rightNeighborOfSuccessor) % (fareyIndex * denominator successor - denominator rightNeighborOfSuccessor)
      else -- numerator successor * m - denominator successor * l < 1

        let fareyIndex = floor (toRational (m + denominator rightNeighborOfSuccessor) / toRational (denominator successor))
         in (fareyIndex * numerator successor - numerator rightNeighborOfSuccessor) % (fareyIndex * denominator successor - denominator rightNeighborOfSuccessor)
  | otherwise = -1 % 10 -- "N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"

successorOfPairOfNeighborsInFml :: Integer -> Integer -> (Rational, Rational) -> Bool -> Rational
-- See Proposition 1.26 (ii) (a) and Table 1.6 of the monograph. Call for instance:
-- ghci> successorOfPairOfNeighborsInFml 6 4 (3 % 4, 4 % 5) True
-- to get the result:
-- 1 % 1
-- Also call:
-- ghci>  successorOfPairOfNeighborsInFml 6 4 (3 % 4, successorInFml 6 4 (3 % 4)) False
-- to get the same result:
-- 1 % 1
successorOfPairOfNeighborsInFml m l (leftNeighborOfPredecessor, predecessor) checkPair
  -- If checkPair == True, then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Fml
  | m < 2 = -1 % 1 -- "N/A: Parameter m of the sequence should be > 1"
  | l <= 0 || l >= m = -1 % 2 --"N/A: Parameter l should be between 0 (excluded) and m (excluded)"
  | leftNeighborOfPredecessor >= predecessor = -1 % 3 -- "N/A: We should have leftNeighborOfPredecessor < predecessor"
  | predecessor <= 0 % 1 || predecessor >= 1 % 1 = -1 % 4 --"N/A: predecessor should be between (0 % 1) (excluded) and (1 % 1) (excluded)"
  | denominator predecessor > m = -1 % 5 --"N/A: Denominator of the predecessor should not exceed the parameter m of the sequence"
  | l < numerator predecessor = -1 % 6 --"N/A: Numerator of the predecessor should be between 1 (included) and l (included)"
  | leftNeighborOfPredecessor < 0 % 1 = -1 % 7 --"N/A: leftNeighborOfPredecessor should be between (0 % 1) (included) and predecessor (excluded)"
  | denominator leftNeighborOfPredecessor > m = -1 % 8 --"N/A: Denominator of the leftNeighborOfPredecessor should not exceed the parameter m of the sequence"
  | l < numerator leftNeighborOfPredecessor = -1 % 9 --"N/A: Numerator of the leftNeighborOfPredecessor should be between 1 (included) and l (included)"
  | not checkPair || checkPair && predecessor == successorInFml m l leftNeighborOfPredecessor =
    if denominator predecessor * l - numerator predecessor * m >= 1
      then
        let fareyIndex = floor (toRational (m + denominator leftNeighborOfPredecessor) / toRational (denominator predecessor))
         in (fareyIndex * numerator predecessor - numerator leftNeighborOfPredecessor) % (fareyIndex * denominator predecessor - denominator leftNeighborOfPredecessor)
      else -- denominator predecessor * l - denominator predecessor * m  < 1

        let fareyIndex = floor (toRational (l + numerator leftNeighborOfPredecessor) / toRational (numerator predecessor))
         in (fareyIndex * numerator predecessor - numerator leftNeighborOfPredecessor) % (fareyIndex * denominator predecessor - denominator leftNeighborOfPredecessor)
  | otherwise = -1 % 10 -- "N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"

predecessorOfPairOfNeighborsInGml :: Integer -> Integer -> (Rational, Rational) -> Bool -> Rational
-- See Proposition 1.27 (ii) (a) and Table 1.6 of the monograph. Call for instance:
-- ghci> predecessorOfPairOfNeighborsInGml 6 4 (1 % 2, 3 % 5) True
-- to get the result:
-- 1 % 3
-- Also call:
-- ghci>  predecessorOfPairOfNeighborsInGml 6 4 (predecessorInGml 6 4 (3 % 5), 3 % 5) False
-- to get the same result:
-- 1 % 3
predecessorOfPairOfNeighborsInGml m l (successor, rightNeighborOfSuccessor) checkPair
  -- If checkPair == True, then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Gml
  | m < 2 = -1 % 1 -- "N/A: Parameter m of the sequence should be > 1"
  | l <= 0 || l >= m = -1 % 2 --"N/A: Parameter l should be between 0 (excluded) and m (excluded)"
  | successor >= rightNeighborOfSuccessor = -1 % 3 -- "N/A We should have successor < rightNeighborOfSuccessor"
  | successor <= 0 % 1 || successor > 1 % 1 = -1 % 4 --"N/A: successor should be between (0 % 1) (excluded) and (1 % 1) (included)"
  | denominator successor > m = -1 % 5 --"N/A: Denominator of the successor should not exceed the parameter m of the sequence"
  | l + denominator successor - m > numerator successor = -1 % 6 --"N/A: The quantity (l + denominator - m) should not exceed the numerator of the successor"
  | rightNeighborOfSuccessor > 1 % 1 = -1 % 7 --"N/A: rightNeighborOfSuccessor should be between successor (excluded) and (1 % 1) (included)"
  | denominator rightNeighborOfSuccessor > m = -1 % 8 --"N/A: Denominator of the rightNeighborOfSuccessor should not exceed the parameter m of the sequence"
  | l + denominator rightNeighborOfSuccessor - m > numerator rightNeighborOfSuccessor = -1 % 9 --"N/A: The quantity (l + denominator - m) should not exceed the numerator of the rightNeighborOfSuccessor"
  | not checkPair || checkPair && successor == predecessorInGml m l rightNeighborOfSuccessor =
    if numerator successor * m - denominator successor * l >= 1
      then
        let fareyIndex = floor (toRational (m + denominator rightNeighborOfSuccessor) / toRational (denominator successor))
         in (fareyIndex * numerator successor - numerator rightNeighborOfSuccessor) % (fareyIndex * denominator successor - denominator rightNeighborOfSuccessor)
      else -- numerator successor * m - denominator successor * l < 1

        let fareyIndex = floor (toRational (m - l + denominator rightNeighborOfSuccessor - numerator rightNeighborOfSuccessor) / toRational (denominator successor - numerator successor))
         in (fareyIndex * numerator successor - numerator rightNeighborOfSuccessor) % (fareyIndex * denominator successor - denominator rightNeighborOfSuccessor)
  | otherwise = -1 % 10 -- "N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"

successorOfPairOfNeighborsInGml :: Integer -> Integer -> (Rational, Rational) -> Bool -> Rational
-- See Proposition 1.27 (ii) (b) and Table 1.6 of the monograph. Call for instance:
-- ghci> successorOfPairOfNeighborsInGml 6 4 (1 % 3, 1 % 2) True
-- to get the result:
-- 3 % 5
-- Also call:
-- ghci>  successorOfPairOfNeighborsInGml 6 4 (1 % 3, successorInGml 6 4 (1 % 3)) False
-- to get the same result:
-- 3 % 5
successorOfPairOfNeighborsInGml m l (leftNeighborOfPredecessor, predecessor) checkPair
  -- If checkPair == True, then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Gml
  | m < 2 = -1 % 1 -- "N/A: Parameter m of the sequence should be > 1"
  | l <= 0 || l >= m = -1 % 2 --"N/A: Parameter l should be between 0 (excluded) and m (excluded)"
  | leftNeighborOfPredecessor >= predecessor = -1 % 3 -- "N/A: We should have leftNeighborOfPredecessor < predecessor"
  | predecessor <= 0 % 1 || predecessor >= 1 % 1 = -1 % 4 --"N/A: predecessor should be between (0 % 1) (excluded) and (1 % 1) (excluded)"
  | denominator predecessor > m = -1 % 5 --"N/A: Denominator of the predecessor should not exceed the parameter m of the sequence"
  | l + denominator predecessor - m > numerator predecessor = -1 % 6 --"N/A: The quantity (l + denominator - m) should not exceed the numerator of the predecessor"
  | leftNeighborOfPredecessor < 0 % 1 = -1 % 7 --"N/A: leftNeighborOfPredecessor should be between (0 % 1) (included) and predecessor (excluded)"
  | denominator leftNeighborOfPredecessor > m = -1 % 8 --"N/A: Denominator of the leftNeighborOfPredecessor should not exceed n"
  | l + denominator leftNeighborOfPredecessor - m > numerator leftNeighborOfPredecessor = -1 % 9 --"N/A: The quantity (l + denominator - m) should not exceed the numerator of the predecessor"
  | not checkPair || checkPair && predecessor == successorInGml m l leftNeighborOfPredecessor =
    if denominator predecessor * l - numerator predecessor * m >= 1
      then
        let fareyIndex = floor (toRational (m - l + denominator leftNeighborOfPredecessor - numerator leftNeighborOfPredecessor) / toRational (denominator predecessor - numerator predecessor))
         in (fareyIndex * numerator predecessor - numerator leftNeighborOfPredecessor) % (fareyIndex * denominator predecessor - denominator leftNeighborOfPredecessor)
      else -- denominator predecessor * l - numerator predecessor * m < 1

        let fareyIndex = floor (toRational (m + denominator leftNeighborOfPredecessor) / toRational (denominator predecessor))
         in (fareyIndex * numerator predecessor - numerator leftNeighborOfPredecessor) % (fareyIndex * denominator predecessor - denominator leftNeighborOfPredecessor)
  | otherwise = -1 % 10 -- "N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"

predecessorOfPairOfNeighborsInFBnm :: Integer -> Integer -> (Rational, Rational) -> Bool -> Rational
-- See Proposition 1.28 (ii) (a) and Table 1.6 of the monograph. Call for instance:
-- ghci> predecessorOfPairOfNeighborsInFBnm 6 4 (4 % 5, 1 % 1) True
-- to get the result:
-- 3 % 4
-- Also call:
-- ghci>  predecessorOfPairOfNeighborsInFBnm 6 4 (predecessorInFBnm 6 4 (1 % 1), 1 % 1) False
-- to get the same result:
-- 3 % 4
predecessorOfPairOfNeighborsInFBnm n m (successor, rightNeighborOfSuccessor) checkPair
  -- If checkPair == True, then to check whether the input pair is indeed a pair of neighboring fractions in the sequence FBnm
  | n < 2 = -1 % 1 -- "N/A: Parameter n of the sequence should be > 1"
  | m < 1 || m >= n = -1 % 2 -- "N/A: Parameter m of the sequence should be between 0 (excluded) and n (excluded)"
  | successor >= rightNeighborOfSuccessor = -1 % 3 -- "N/A We should have successor < rightNeighborOfSuccessor"
  | successor <= 0 % 1 || successor >= 1 % 1 = -1 % 4 --"N/A: successor should be between (0 % 1) (excluded) and (1 % 1) (excluded)"
  | denominator successor > n = -1 % 5 --"N/A: Denominator of the successor should not exceed n"
  | m + denominator successor - n > numerator successor || numerator successor > m = -1 % 6 --"N/A: Numerator of the successor should be between (m + denominator - n) (included) and m (included)"
  | rightNeighborOfSuccessor > 1 % 1 = -1 % 7 --"N/A: rightNeighborOfSuccessor should be between successor (excluded) and (1 % 1) (included)"
  | denominator rightNeighborOfSuccessor > n = -1 % 8 --"N/A: Denominator of the rightNeighborOfSuccessor should not exceed n"
  | m + denominator rightNeighborOfSuccessor - n > numerator rightNeighborOfSuccessor || numerator rightNeighborOfSuccessor > m = -1 % 9 --"N/A: Numerator of the rightNeighborOfSuccessor should be between (m + denominator - n) (included) and m (included)"
  | not checkPair || checkPair && successor == predecessorInFBnm n m rightNeighborOfSuccessor =
    if numerator successor * n - denominator successor * m >= 1
      then
        let fareyIndex = floor (toRational (m + numerator rightNeighborOfSuccessor) / toRational (numerator successor))
         in (fareyIndex * numerator successor - numerator rightNeighborOfSuccessor) % (fareyIndex * denominator successor - denominator rightNeighborOfSuccessor)
      else -- numerator successor * n - denominator successor * m < 1

        let fareyIndex = floor (toRational (n - m + denominator rightNeighborOfSuccessor - numerator rightNeighborOfSuccessor) / toRational (denominator successor - numerator successor))
         in (fareyIndex * numerator successor - numerator rightNeighborOfSuccessor) % (fareyIndex * denominator successor - denominator rightNeighborOfSuccessor)
  | otherwise = -1 % 10 -- "N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"

predecessorOfPairOfNeighborsInFB2mm :: Integer -> (Rational, Rational) -> Bool -> Rational
predecessorOfPairOfNeighborsInFB2mm m (successor, rightNeighborOfSuccessor) checkPair =
  predecessorOfPairOfNeighborsInFBnm (2 * m) m (successor, rightNeighborOfSuccessor) checkPair

successorOfPairOfNeighborsInFBnm :: Integer -> Integer -> (Rational, Rational) -> Bool -> Rational
-- See Proposition 1.28 (ii) (b) and Table 1.6 of the monograph. Call for instance:
-- ghci> successorOfPairOfNeighborsInFBnm 6 4 (1 % 3, 1 % 2) True
-- to get the result:
-- 3 % 5
-- Also call: successorOfPairOfNeighborsInFBnm 6 4 (1 % 3, successorInFBnm 6 4 (1 % 3)) False
-- ghci>
-- to get the same result:
-- 3 % 5
successorOfPairOfNeighborsInFBnm n m (leftNeighborOfPredecessor, predecessor) checkPair
  -- If checkPair == True, then to check whether the input pair is indeed a pair of neighboring fractions in the sequence FBnm
  | n < 2 = -1 % 1 -- "N/A: Parameter n of the sequence should be > 1"
  | m < 1 || m >= n = -1 % 2 -- "N/A: Parameter m of the sequence should be between 0 (excluded) and n (excluded)"
  | leftNeighborOfPredecessor >= predecessor = -1 % 3 -- "N/A: We should have leftNeighborOfPredecessor < predecessor"
  | predecessor >= 1 % 1 || predecessor <= 0 % 1 = -1 % 4 --"N/A: predecessor should be between (0 % 1) (excluded) and (1 % 1) (excluded)"
  | denominator predecessor > n = -1 % 5 --"N/A: Denominator of the predecessor should not exceed n"
  | m + denominator predecessor - n > numerator predecessor || numerator predecessor > m = -1 % 6 --"N/A: Numerator of the predecessor should be between (m + denominator - n) (included) and m (included)"
  | leftNeighborOfPredecessor < 0 % 1 = -1 % 7 --"N/A: leftNeighborOfPredecessor should be between 0%1 (included) and predecessor (excluded)"
  | denominator leftNeighborOfPredecessor > n = -1 % 8 --"N/A: Denominator of the leftNeighborOfPredecessor should not exceed n"
  | m + denominator leftNeighborOfPredecessor - n > numerator leftNeighborOfPredecessor || numerator leftNeighborOfPredecessor > m = -1 % 9 --"N/A: Numerator of the leftNeighborOfPredecessor should be between (m + denominator - n) (included) and m (included)"
  | not checkPair || checkPair && predecessor == successorInFBnm n m leftNeighborOfPredecessor =
    if denominator predecessor * m - numerator predecessor * n >= 1
      then
        let fareyIndex = floor (toRational (n - m + denominator leftNeighborOfPredecessor - numerator leftNeighborOfPredecessor) / toRational (denominator predecessor - numerator predecessor))
         in (fareyIndex * numerator predecessor - numerator leftNeighborOfPredecessor) % (fareyIndex * denominator predecessor - denominator leftNeighborOfPredecessor)
      else -- denominator predecessor * m - numerator predecessor * n < 1

        let fareyIndex = floor (toRational (m + numerator leftNeighborOfPredecessor) / toRational (numerator predecessor))
         in (fareyIndex * numerator predecessor - numerator leftNeighborOfPredecessor) % (fareyIndex * denominator predecessor - denominator leftNeighborOfPredecessor)
  | otherwise = -1 % 10 -- "N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"

successorOfPairOfNeighborsInFB2mm :: Integer -> (Rational, Rational) -> Bool -> Rational
successorOfPairOfNeighborsInFB2mm m (leftNeighborOfPredecessor, predecessor) checkPair =
  successorOfPairOfNeighborsInFBnm (2 * m) m (leftNeighborOfPredecessor, predecessor) checkPair
