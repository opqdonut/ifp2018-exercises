module W6Test where

import W6

import Data.List
import Data.Char
import Data.Either
import Data.Ord
import Control.Monad.Trans.State

import Impl.Test
import Test.QuickCheck hiding (Result,reason,classify,Failure,(===))

main = testExs tests

tests :: [[Property]]
tests = [[property ex1_ok, property ex1_fail]
        ,[property ex2_ok, property ex2_fail]
        ,[property ex3_ok, property ex3_fail]
        ,[property ex4_1, property ex4_2, property ex4_3]
        ,[property ex5]
        ,[property ex6]
        ,[property ex7_add, property ex7_remove]
        ,[property ex8]
        ,[property ex9_Maybe, property ex9_State]
        ,[property ex10]
        ,[property ex11_dfs_1, property ex11_dfs_2
         ,property ex11_routeExists_basic, property ex11_routeExists]
        ,[property ex11_dfs_1, property ex11_dfs_2
         ,property ex11_routeExists_basic, property ex11_routeExists]
        ,[property ex13_1, property ex13_2]
        ,[property ex14]
        ,[property ex15_sumNotTwice]
        ,[property ex16]
        ,[property ex17_1, property ex17_2, property ex17_stress]
        ,[property ex17_1, property ex17_2, property ex17_stress]
        ]

-- -- -- -- -- -- -- --

word = do fst <- choose ('A','Z')
          rest <- listOf (choose ('a','z'))
          return $ fst:rest

ex1_ok = do
  for <- word
  sur <- word
  let str = for++" "++sur
  counterexample' ("readNames "++show str) $
    readNames str === Just (for,sur)

m_ex1_fail s =
  counterexample ("readNames "++show s) $ readNames s === Nothing

ex1_fail =
  do for <- word
     sur <- word
     return $ conjoin [m_ex1_fail (for ++ sur),
                       m_ex1_fail (map toLower for ++ " " ++ sur),
                       m_ex1_fail (for ++ " " ++ map toLower sur),
                       m_ex1_fail (for ++ " " ++ for)]

ex2_ok = do
  as <- listOf arbitrary :: Gen [Int]
  i <- choose (0,length as)
  let ml = Just as
      mi = Just i
  counterexample' ("myDrop ("++show mi++") ("++show ml++")") $
    myDrop mi ml === Just (drop i as)

ex2_fail = do
  as <- listOf arbitrary :: Gen [Int]
  i <- choose (length as+1,length as+5)
  let ml = Just as
      mi = Just i
      neg = Just (negate i)
  return $ conjoin [counterexample' ("myDrop ("++show mi++") ("++show ml++")") $
                    myDrop mi ml === Nothing,
                    counterexample' ("myDrop ("++show neg++") ("++show ml++")") $
                    myDrop neg ml === Nothing,
                    counterexample' ("myDrop Nothing ("++show ml++")") $
                    myDrop Nothing ml === Nothing,
                    counterexample' ("myDrop ("++show mi++") Nothing") $
                    myDrop mi (Nothing :: Maybe String) === Nothing]

ex3_ok = do
  as <- listOf1 arbitrary :: Gen [Integer]
  is <- listOf (choose (0,length as - 1))
  counterexample' ("selectSum "++show as++" "++show is) $
    selectSum as is === Just (sum $ map (as!!) is)

ex3_fail = do
  as <- arbitrary :: Gen [Int]
  is1 <- listOf (choose (0,length as - 1))
  is2 <- listOf (choose (0,length as - 1))
  b <- elements [-1,length as]
  let is = is1++b:is2
  counterexample' ("selectSum "++show as++" "++show is) $
    selectSum as is === Nothing

b n k = case (n,k) of (_,0) -> 1
                      (0,_) -> 0
                      (n,k) -> b (n-1) (k-1) + b (n-1) k

ex4_1 = do
  n <- choose (0,7)
  k <- choose (0,n)
  let Logger _ res = binom n k
  counterexample' ("Return value of binom "++show n++" "++show k) $
    res === b n k

ex4_2 = do
  n <- choose (0,7)
  k <- choose (0,n)
  let Logger log _ = binom n k
  counterexample' ("Log of binom "++show n++" "++show k) $
    conjoin [counterexample' "log should not be empty" $
             not $ null log,
             counterexample' "last message of log" $
             last log === ("B("++show n++","++show k++")"),
             counterexample' "first message of log" $
             head log === ("B("++show (n-k)++",0)")]



ex4_3 =
  conjoin [t 2 2 ["B(0,0)","B(0,1)","B(1,1)","B(0,1)","B(0,2)","B(1,2)","B(2,2)"],
           t 2 7 ["B(0,5)","B(0,6)","B(1,6)","B(0,6)","B(0,7)","B(1,7)","B(2,7)"],
           t 3 3 ["B(0,0)","B(0,1)","B(1,1)","B(0,1)","B(0,2)","B(1,2)","B(2,2)","B(0,1)","B(0,2)","B(1,2)","B(0,2)","B(0,3)","B(1,3)","B(2,3)","B(3,3)"],
           t 4 3 ["B(1,0)","B(0,0)","B(0,1)","B(1,1)","B(2,1)","B(0,0)","B(0,1)","B(1,1)","B(0,1)","B(0,2)","B(1,2)","B(2,2)","B(3,2)","B(0,0)","B(0,1)","B(1,1)","B(0,1)","B(0,2)","B(1,2)","B(2,2)","B(0,1)","B(0,2)","B(1,2)","B(0,2)","B(0,3)","B(1,3)","B(2,3)","B(3,3)","B(4,3)"]]
  where t n k log = counterexample' ("binom "++show n++" "++show k) $ let Logger l _ = binom n k in l===log

ex5 i = counterexample' ("runState update "++show i) $
        runState update i === ((),2*i+1)

ex6 bs = counterexample' ("runState (lengthAndSum "++show bs++") 0") $
         runState (lengthAndSum bs) 0 === (length bs, sum bs)

sortSnd (a,xs) = (a, sort xs)

ex7_add = do
  (x:state) <- fmap nub $ listOf1 (choose (1,20::Integer))
  counterexample' ("runState (oddUpdate "++show x++") "++show state) $
                  sortSnd (runState (oddUpdate x) state) === ((), sort (x:state))

ex7_remove = do
  state <- fmap nub $ listOf1 (choose (1,20::Int))
  x <- elements state
  counterexample' ("runState (oddUpdate "++show x++") "++show state) $
                  sortSnd (runState (oddUpdate x) state) === ((), sort (delete x state))

ex8 xs = counterexample' ("odds "++show xs) $
         sort (odds xs) === odds' xs
    where odds' :: [Int] -> [Int]
          odds' xs = concatMap keep . group $ sort xs
          keep xs
              | odd (length xs) = [head xs]
              | otherwise = []

ex9_Maybe :: Maybe Bool -> Maybe Int -> Maybe Int -> Property
ex9_Maybe b t e = ifM b t e === case b of Just True -> t
                                          Just False -> e
                                          Nothing -> Nothing

ex9_State = do
  b <- arbitrary
  t <- choose (1,1024 :: Int)
  e <- choose (1,1024 :: Int)
  counterexample' ("runState (ifM (return "++show b++") (modify (+ "++show t++")) (modify (+ "++show e++"))) 0") $
    runState (ifM (return b) (modify (+t)) (modify (+e))) 0 === ((), if b then t else e)

ex10 :: [Int] -> [Int] -> Property
ex10 as bs =
  counterexample ("mapM2 (\\x y -> if x == y then Nothing else Just (x-y)) "++show as++" "++show bs) $
    mapM2 (\x y -> if x == y then Nothing else Just (x-y)) as bs === res
  where z = zipWith (-) as bs
        res = if all (/=0) z then Just z else Nothing

ex11_dfs_1 = do
  let cs = [[1],[0,2],[1,3],[2,4],[3,5],[4]]
  i <- choose (1,length cs - 1)
  let st = [0..i-1]
  counterexample' ("runState (dfs "++show cs++" "++show i++") "++show st) $
    let ((),res) = runState (dfs cs i) st
    in sort res === [0..5]

ex11_dfs_2 = do
  let cs = [[1,4],[0,2],[1,3],[2,4],[3,0]]
  i <- choose (1,length cs - 1)
  counterexample' ("runState (dfs "++show cs++" "++show i++") []") $
    let ((),res) = runState (dfs cs i) []
    in sort res === [0..4]

ex11_routeExists_basic = do
  siz <- choose (2,5)
  let cs = map (\i -> delete i [0..siz-1]) [0..siz-1]
  a <- choose (0,siz-1)
  b <- choose (0,siz-1)
  counterexample' ("routeExists "++show cs++" "++show a++" "++show b) $
    routeExists cs a b === True

shuffle xs = do
  is <- vector (length xs) :: Gen [Int]
  return $ map snd . sortBy (comparing fst) $ zip is xs


genGraph' :: [Int] -> [Int] -> [(Int,Int)] -> Gen [(Int,Int)]
genGraph' is [] es = return es
genGraph' is todo es = do
  u <- elements $ todo
  v <- elements $ is \\ todo
  genGraph' is (delete u todo) ((u,v):(v,u):es)

genGraph :: [Int] -> Gen [(Int,Int)]
genGraph is = do
  base <- genGraph' is (tail is) []
  [a,b,c] <- vectorOf 3 (elements is)
  return $ (a,b):(b,c):base

mkGraph es = map neighs [0..n]
  where n = maximum (map fst es ++ map snd es)
        neighs i = nub $ sort $ map snd $ filter (\(x,_) -> x==i) es

ex11_routeExists = do
  siz <- choose (5,7)
  k <- choose (2,siz-2)
  left <- genGraph [0..k]
  right <- genGraph [k+1..siz-1]
  i <- choose (0,siz-1)
  j <- choose (0,siz-1)
  let cities = mkGraph (left++right)
  counterexample' (show left++"\n"++show right++"\n"++"routeExists "++show cities++" "++show i++" "++show j) $
    routeExists cities i j === ((i<=k) == (j<=k))

m is = maximum (scanl1 (+) is)

ex13_1 = do
  let n = 6
  is <- vectorOf n (choose (0,10))
  i <- choose (0,n-2)
  j <- choose (i+1,n-1)
  let a = is!!i
      b = is!!j
      ret = orderedPairs is
  counterexample' ("orderedPairs "++show is) $
    if a<b
    then counterexample' ("The pair "++show (a,b)++" should be in the list.") $ (a,b) `elem` ret
    else counterexample' ("The pair "++show (a,b)++" should not be in the list.") . not $ (a,b) `elem` ret

ex13_2 = do
  let n = 7
  let is0 = [0..n]
  x <- choose (0,n)
  let is = drop x is0 ++ take x is0
      exp = [(i,j) | i<-[0..x-2], j<-[i+1..x-1]]
            ++
            [(i,j) | i<-[x..n-1], j<-[i+1..n]]
  counterexample' ("orderedPairs "++show is) $
    sort (orderedPairs is) === sort exp

sums' [] = [0]
sums' (x:xs) = sums' xs ++ map (x+) (sums' xs)

ex14 is = counterexample' ("pairs "++show input) $
          sort (pairs input) === sort (pairs' input)
    where input = nub is
          pairs' :: [Integer] -> [(Integer,Integer)]
          pairs' [] = []
          pairs' (x:xs) = map (\y -> (x,y)) xs ++ map (\y -> (y,x)) xs ++ pairs' xs

ex15_sumNotTwice is =
  sumNotTwice is === sum (nub is)

ex16 =
  let op :: Int -> Result Int
      op i = if i>3 then fail "big" else return (i+1)
      s = "let op i = if (i>3) then fail \"big\" else return (i+1) in "
  in conjoin [counterexample' (s++" MkResult 1 >>= op") $
              (MkResult 1 >>= op) === MkResult 2,
              counterexample' (s++" MkResult 4 >>= op") $
              (MkResult 4 >>= op) === Failure "big",
              counterexample' (s++" Fail \"foo\" >>= op") $
              (Failure "foo" >>= op) === Failure "foo",
              counterexample' (s++" NoResult >>= op") $
              (NoResult >>= op) === NoResult]

ex17_fmap_1 =
  do i <- choose (0,10)
     let op = fmap (+1) getSL
     counterexample' ("runSL (fmap (+1) getSL) " ++ show i) $
       runSL op i === (i,i+1,[])

ex17_fmap_2 =
  do m <- word
     s <- choose (0,10)
     let op = fmap (const True) (msgSL m)
     counterexample' ("runSL (fmap (const True) (msgSL "++show m++")) "++show s) $
       runSL op s === (True,s,[m])

ex17_1 =
  do i <- choose (0,10)
     let op = putSL i >> getSL >>= \i -> msgSL (show i)
         s = "putSL "++show i++" >> getSL >>= \\i -> msgSL (show i)"
     counterexample' ("runSL ("++s++") 1") $ runSL op 1 === ((),i,[show i])

ex17_2 =
  do msg <- word
     msg2 <- word
     i <- choose (0,10)
     j <- choose (0,10)
     let op = do msgSL msg
                 x <- getSL
                 msgSL (msg2++show x)
                 putSL (x+i)
                 return x
         s = "op = \ndo msgSL "++show msg++"\n   x <- getSL\n   msgSL ("++show msg2++"++show x)\n   putSL (x+"++show i++")\n   return x"
     counterexample' (s++"\nrunSL op "++show j) $ runSL op j === (j,j+i,[msg,msg2++show j])

ex17_stress =
  arbitrary >>= \o ->
  return . shrinking shrink o $ \(NonEmpty ops) ->
  let m (Left i) = modifySL (+i)
      m (Right s) = msgSL s
      s (Left i) = "modifySL (+"++show i++")"
      s (Right m) = "msgSL "++show m
      op = mapM_ m ops
      desc = "runSL ("++intercalate " >> " (map s ops)++") 0"
      (incs,msgs) = partitionEithers ops
      state = sum incs
  in counterexample' desc $ runSL op 0 === ((),state,msgs)
