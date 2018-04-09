module W2TestSansHT where

import Impl.Test
import W2
import Data.List
import Data.Char
import Test.QuickCheck hiding ((===))

main = testExs tests

tests = [[]
        ,[property ex2_measure_empty, property ex2_measure_nonEmpty]
        ,[property ex3_takeFinal_1, property ex3_takeFinal_2]
        ,[property ex4_remove]
        ,[property ex5_substring]
        ,[property ex6_mymax]
        ,[property ex7_countPalindromes]
        ,[ex8_funny_1, ex8_funny_2]
        ,[property ex9_powers]
        ,[property ex10_search_number, property ex10_search_string]
        ,[property ex11_fromTo]
        ,[property ex12_sums]
        ,[property ex13_mylast_nonempty, property ex13_mylast_empty]
        ,[property ex14_sorted_empty, property ex14_sorted_sorted]
        ,[property ex15_sumsOf]
        ,[property ex16_merge]
        ,[property ex17_mergesort]
        ,[property ex18_mymaximum_max, property ex18_mymaximum_min, property ex18_mymaximum_empty]
        ,[property ex19_map_1, property ex19_map_2]
        ,[property ex20_interpreter_1, property ex20_interpreter_2]]


-- -- -- -- -- --

ex2_measure_empty () = measure [] === -1
ex2_measure_nonEmpty (NonEmpty xs) = measure xs === length xs

ex3_takeFinal_1 = do
  n <- choose (0,20)
  k <- choose (0,n)
  return $ counterexample ("takeFinal "++show k++" [0.."++show n++"]") $
           takeFinal k [0..n] === [n-k+1..n]

ex3_takeFinal_2 = do
  n <- choose (0,20)
  k <- choose (0,n)
  let inp = reverse [0..n]
  return $ counterexample ("takeFinal "++show k++" "++show inp) $
           takeFinal k inp === reverse [0..k-1]

ex4_remove = do
  n <- choose (0,20)
  k <- choose (0,n)
  let inp = [0,2..2*n]
      out = map (2*) ([0..(k-1)] ++ [k+1..n])
  return $ counterexample ("remove "++show k++" "++show inp) $
           remove k inp === out

ex5_substring = do
  base <- choose (ord 'a',ord 'f')
  len <- choose (0,20)
  let list = f [base..base+len-1]
  i <- choose (0,max 0 (len-1))
  n <- choose (0,len-i)
  return $ counterexample ("substring "++show i++" "++show n++" "++show list) $
    substring i n list === f [base+i .. base + min (i+n) (len) - 1]
  where f = map chr

ex6_mymax = do
  t <- choose (0,20)
  f <- choose (0,20) `suchThat` \f -> f/=t
  let p True = t
      p False = f
  return $
    counterexample ("let p True = "++show t++"; p False = "++show f++" in mymax p False True") $
    mymax p False True === (t>f)

word = listOf1 (choose ('a','z'))
palindrome = fmap (\s -> s ++ reverse (init s)) word
unpalindrome = word `suchThat` \w -> w /= reverse w

ex7_countPalindromes = do
  ss <- listOf1 palindrome
  us <- listOf1 unpalindrome
  k <- choose (1,5)
  let ws = comb k ss us
  return $ counterexample ("countPalindromes "++show ws) $
    countPalindromes ws === length ss
  where comb k [] b = b
        comb k a b = take k a ++ comb k b (drop k a)

ex8_funny_1 =
  counterexample ("funny "++show inp) $
  funny inp === out
  where inp = ["a","bcdefgh","simo","xxxxxxxxxxx"]
        out = "BCDEFGH XXXXXXXXXXX"

ex8_funny_2 =
  counterexample ("funny "++show inp) $
  funny inp === out
  where inp = ["aaaaaa","bbbbbb","ccccc","ddddddd"]
        out = "AAAAAA BBBBBB DDDDDDD"

ex9_powers = do
  n <- choose (2,5)
  len <- choose (1,10)
  end <- choose (n^(len-1),n^len-1)
  let p = powers n end
  return $ counterexample ("powers "++show n++" "++show end) $ conjoin
    [counterexample "all smaller than end" $
     all (<=end) p
    ,counterexample "sorted" $
     p == sort p
    ,counterexample "length" $
     length p === len
    ,counterexample "powers of n" $
     all (check n) p]
  where check n 0 = True
        check n 1 = True
        check n k
          | k `mod` n == 0    = check n (div k n)
          | otherwise         = False

ex10_search_number = do
  n <- choose (0,20 :: Integer)
  return $ counterexample ("search (+1) (=="++show n++") 0") $
    search (+1) (==n) 0 === n

ex10_search_string = do
  n <- word
  let w = n++n
      p = (==n)
  return $ counterexample ("search tail (=="++show n++") "++show w) $
    search tail p w == n



ex11_fromTo = do
  start <- choose (0,20)
  len <- choose (0,10)
  let end = start+len-1
  return $ counterexample ("fromTo "++show start++" "++show end) $
    fromTo start end === [start..end]

ex12_sums = do
  i <- choose (1,20)
  return $ counterexample ("sums "++show i) $
    sums i === scanl1 (+) [1..i]


ex13_mylast_nonempty :: NonEmptyList Integer -> Property
ex13_mylast_nonempty (NonEmpty xs) = mylast 0 xs === last xs
ex13_mylast_empty :: Char -> Property
ex13_mylast_empty i = mylast i [] === i

ex14_sorted_empty =
    counterexample "sorted []" $ sorted [] === True

ex14_sorted_sorted = do
  l <- vector 5
  let s = sort l
  return $ conjoin
    [counterexample ("sorted "++show l) $ sorted l === (s == l)
    ,counterexample ("sorted "++show s) $ sorted s === True]

ex15_sumsOf xs = sumsOf xs === scanl1 (+) xs

ex16_merge = do
  xs <- fmap sort arbitrary
  ys <- fmap sort arbitrary
  return $ counterexample ("merge "++show xs++" "++show ys) $
    merge xs ys == sort (xs ++ ys)

ex17_mergesort xs = mergesort xs === sort xs

ex18_mymaximum_max :: NonEmptyList Integer -> Property
ex18_mymaximum_max (NonEmpty xs) = mymaximum compare 0 xs === maximum xs

ex18_mymaximum_min :: NonEmptyList Integer -> Property
ex18_mymaximum_min (NonEmpty xs) =
    counterexample ("mymaximum (\\x y -> compare y x) 0 " ++ show xs) $
    mymaximum (\x y -> compare y x) 0 xs === minimum xs

ex18_mymaximum_empty = do
  i <- choose (True,False)
  return $ counterexample ("mymaximum compare "++show i++" []") $ mymaximum compare i [] === i

ex19_map_1 = do
  i <- arbitrary :: Gen [Int]
  j <- arbitrary :: Gen [Bool]
  return $ counterexample ("map2 const "++show i++" "++show j) $
    map2 const i j === take (length j) i

ex19_map_2 = do
  i <- arbitrary :: Gen [Int]
  j <- arbitrary :: Gen [Int]
  return $ counterexample ("map2 (+) "++show i++" "++show j) $
    map2 (+) i j === zipWith (+) i j


ex20_interpreter_1 = do
  up <- choose (10,20)
  right <- choose (0,10)
  down <- choose (1,3)
  left <- choose (0,1)

  let first = replicate up "up" ++ replicate right "right" ++ ["printY","printX"]
      second = replicate down "down" ++ replicate left "left" ++ ["printY","printX"]
      input = first ++ second
      output = [show up, show right, show (up-down), show (right-left)]

  return $ counterexample ("interpreter "++show input) $
    interpreter input === output

ex20_interpreter_2 = do
  nums <- vectorOf 4 $ choose (0,10)
  let diffs = zipWith (-) nums (0:nums)
      f x | x<0 = replicate (negate x) "down"
          | otherwise = replicate x "up"
      input = concatMap (\x -> f x ++ ["printY"]) diffs
      output = map show nums
  return $ counterexample ("interpreter "++show input) $
    interpreter input === output
