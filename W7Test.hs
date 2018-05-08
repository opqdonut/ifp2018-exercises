module W7Test where

import W7

import Data.List
import Data.Char
import Data.Either
import Data.Ord
import Control.Monad.Trans.State

import Impl.Test
import Test.QuickCheck hiding (Result,reason,classify,Failure,(===))

main = testExs tests

tests :: [[Property]]
tests = [[ex1]
        ,[ex2_small, ex2_large]
        ,[ex3]
        ,[ex4]
        ,[ex5]
        ,[property ex6_circle, property ex6_rectangle]
        ,[ex7]
        ,[property ex8_simple]
        ,[property ex9]
        ,[property ex10_functor, property ex10_return, property ex10_bind_1]
        ]

ex1 = property $ do
  low <- choose (0,10)
  high <- choose (low,15)
  range <- listOf (choose (low,high))
  outliers <- listOf (oneof [choose (-10,low-1), choose (high+1,20)])
  input <- shuffle (range++outliers)
  counterexample' ("countRange "++show low++" "++show high++" "++show input) $
                  countRange low high input === length range

ex2_small = property $
            conjoin [counterexample "chess 1 1" $ chess 1 1 === "#\n"
                    ,counterexample "chess 3 5" $ chess 3 5 === "#.#.#\n.#.#.\n#.#.#\n"]

evens [] = []
evens (x:xs) = odds xs

odds [] = []
odds (x:xs) = x:evens xs

allEqual [] = True
allEqual (x:xs) = all (==x) xs

ex2_large = property $ do
  i <- choose (2,10)
  j <- choose (2,10)
  let out = chess i j
      ls = lines out
  counterexample' ("chess "++show i++" "++show j) $
    conjoin [counterexample "number of lines" $ length ls === i
            ,counterexample "length of lines" $ map length ls === replicate i j
            ,counterexample "characters" $ sort (nub out) === sort ".#\n"
            ,counterexample "even lines should match" $ allEqual (evens ls)
            ,counterexample "odd lines should match" $ allEqual (odds ls)]

character = choose ('a','z')
word = listOf1 character

ex3 = property $ do
        beef <- word
        center <- sublistOf "z"
        bun <- fmap nub word `suchThat` \w -> length w > 1
        let answer = beef ++ center ++ reverse beef
            input = bun ++ answer ++ bun
        counterexample' ("palindromify "++show input) $ palindromify input === answer

ex4 = property $ do
  l <- listOf $ choose (0,5::Int)
  counterexample' ("unrepeat "++show l) $
      unrepeat l === map head (group l)

ex5 = property $ do
  nums <- listOf1 arbitrary :: Gen [Int]
  strings <- arbitrary :: Gen [String]
  let empty = map Left strings :: [Either String Int]
  input <- shuffle (empty ++ map Right nums)
  return $ conjoin [counterexample ("sumEithers "++show empty) $ sumEithers empty === Nothing
                   ,counterexample ("sumEithers "++show input) $ sumEithers input === Just (sum nums)]

eps = 0.001

almostEqual x y = abs (x-y) < eps

ex6_circle r =
  counterexample ("area (circle "++show r++") is almost " ++ show answer) $
  almostEqual answer (area (circle r))
      where answer = pi * r * r

ex6_rectangle w h =
  counterexample ("area (rectangle "++show w++" "++show h++") is almost " ++ show answer) $
  almostEqual answer (area (rectangle w h))
      where answer = w*h

bo x op y = show x ++ " " ++ op ++ " " ++ show y

ex7 = property $ do
  i <- choose (1,13)
  j <- choose (1,13) `suchThat` \j -> j /= i
  let hi = Heart i
      hj = Heart j
      si = Spade i
      sj = Spade j
  return $ conjoin [counterexample (bo hi "==" hj) $ (hi == hj) === False
                   ,counterexample (bo hi "==" hi) $ (hi == hi) === True
                   ,counterexample (bo si "==" sj) $ (si == sj) === False
                   ,counterexample (bo si "==" si) $ (si == si) === True
                   ,counterexample (bo Joker "==" Joker) $ (Joker == Joker) === True
                   ,counterexample (bo si "==" Joker) $ (si == Joker) === False
                   ,counterexample (bo Joker "==" hi) $ (Joker == hi) === False
                   ,counterexample (bo hi "<=" hj) $ (hi <= hj) === (i <= j)
                   ,counterexample (bo si "<=" sj) $ (si <= sj) === (i <= j)
                   ,counterexample (bo si "<=" hj) $ (si <= hj) === True
                   ,counterexample (bo hi "<=" sj) $ (hi <= sj) === False
                   ,counterexample (bo hi "<=" Joker) $ (hi <= Joker) === True
                   ,counterexample (bo Joker "<=" hi) $ (Joker <= hi) === False
                   ,counterexample (bo si "<=" Joker) $ (si <= Joker) === True]

ex8_simple a b c d =
    counterexample ("fmap succ "++show input) $ fmap succ input === output
    where input = Continue (a::Int) b (End c d)
          output = Continue (succ a) (succ b) (End (succ c) (succ d))

ex9 is =
    counterexample ("sumEvens "++show is) $
    execState (sumEvens is) 0 === sum (filter even is)

ex10_functor s =
    counterexample ("runEnv (fmap (*2) envLength) "++show s) $
    runEnv (fmap (*2) envLength) s === 2 * length s

ex10_return x s =
    counterexample ("runEnv (return " ++ show x ++") "++show s) $
    runEnv (return x) s === (x :: Char)

ex10_bind_1 s =
    counterexample ("runEnv (envLength >>= multiply) "++show s) $
    runEnv (envLength >>= multiply) s === concat (replicate (length s) s)
