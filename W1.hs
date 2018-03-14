module W1 where

-- Week 1:
--   * defining functions
--   * basic expressions
--   * pattern matching
--   * recursion

-- Ex 1: define variables one, two and three. They should all have
-- type Int and values 1, 2 and 3. This exercise has no tests.

-- Ex 2: define the function double of type Integer->Integer. Double
-- should take one argument and return it multiplied by two.

double :: Integer -> Integer
double x = undefined

-- Ex 3: define the function quadruple that uses the function double
-- from the previous exercise to return its argument multiplied by
-- four.

quadruple :: Integer -> Integer
quadruple x = undefined

-- Ex 4: define the function poly2. It should take four arguments of
-- type Double, a, b, c, and x and return a*x^2+b*x+c. Give poly2 a
-- type signature, i.e. poly2 :: something.

poly2 = undefined

-- Ex 5: define the function eeny that returns "eeny" for even inputs
-- and "meeny" for odd inputs.
--
-- Ps. have a look at the built in function "even"

eeny :: Integer -> String
eeny = undefined

-- Ex 6: fizzbuzz! Define the a function fizzbuzz that returns "Fizz"
-- for numbers divisible by 3, "Buzz" for numbers divisible by 5, and
-- "FizzBuzz" for numbers divisible by both. For other numbers it
-- returns the empty string.
--
-- You can use the function mod to compute modulo.

fizzbuzz = undefined

-- Ex 7: define a function isZero that returns True if it is given an
-- Integer that is 0, and False otherwise. Give isZero a type signature.
--
-- Use pattern matching! Don't use comparisons!
--
-- Ps. the type of booleans in haskell is Bool

isZero = undefined

-- Ex 8: implement using recursion a function sumTo such that
--   sumTo n
-- computes the sum 1+2+...+n

sumTo :: Integer -> Integer
sumTo = undefined

-- Ex 9: power n k should compute n to the power k (i.e. n^k)
-- Use recursion.

power :: Integer -> Integer -> Integer
power = undefined

-- Ex 10: ilog2 n should be the number of times you can halve the
-- integer n (rounding down) before you get 1.
--
-- Use recursion to define ilog2. Use the function "div" for integer
-- division.

ilog2 :: Integer -> Integer
ilog2 = undefined

-- Ex 11: compute binomial coefficients using recursion. Binomial
-- coefficients are defined by the following equations:
--
--   B(n,k) = B(n-1,k) + B(n-1,k-1)
--   B(n,0) = 1
--   B(0,k) = 0, when k>0
--
-- Hint! pattern matching is your friend.

binomial :: Integer -> Integer -> Integer
binomial = undefined

-- Ex 12: The tribonacci numbers are defined by the equations
--
--   T(1) = 1
--   T(2) = 1
--   T(3) = 2
--   T(n+1) = T(n)+T(n-1)+T(n-2)
--
-- Implement an efficient, linear time, recursive function that
-- computes T(n). You'll probably want to define a helper function.

tribonacci :: Integer -> Integer
tribonacci = undefined

-- Ex 13: implement the euclidean algorithm for finding the greatest
-- common divisor: http://en.wikipedia.org/wiki/Euclidean_algorithm

myGcd :: Integer -> Integer -> Integer
myGcd = undefined

-- Ex 14: The Haskell Prelude (standard library) defines the type
-- Ordering with values LT, GT and EQ. You try out Ordering by
-- evaluating the following expressions:
--
--   compare 3 4
--   compare 4 3
--   compare 0 0
--   compare "Hei" "Moi"
--   compare True False
--
-- Your task is to implement the function funnyCompare, that orders
-- integers in the following way:
--
-- 1. All even numbers come before odd numbers
-- 2. Within even and odd numbers the ordering is normal
--
-- In other words,
-- odd numbers compare normally:
--   funnyCompare 5 7 ==> LT
-- even numbers compare normally:
--   funnyCompare 8 6 ==> GT
-- even comes before odd:
--   funnyCompare 8 3 ==> LT
--   funnyCompare 2 3 ==> LT

funnyCompare :: Int -> Int -> Ordering
funnyCompare = undefined

-- Ex 15: Implement the function funnyMin that returns the minimum of
-- its two arguments, according to the ordering implemented by
-- funnyCompare.
--
-- Use pattern matching on the Ordering value returned by
-- funnyCompare. To do this, you need to either use the case-of
-- expression or define a helper function.

funnyMin :: Int -> Int -> Int
funnyMin = undefined

-- Ex 16: implement the recursive function pyramid that returns
-- strings like this:
--
-- pyramid 0 ==> "0"
-- pyramid 1 ==> "0,1,0"
-- pyramid 2 ==> "0,1,2,1,0"
-- pyramid 3 ==> "0,1,2,3,2,1,0"
--
-- Hints:
-- * you can glue strings together with the operator ++
-- * the function show transforms a number into a string
-- * you'll need a (recursive) helper function

pyramid :: Integer -> String
pyramid = undefined

-- Ex 17: implement the function smallestDivisor that returns the
-- smallest number (greater than 1) that divides the given number.
--
-- That is, when
--   smallestDivisor n ==> k
-- we have
--   n = t*k
-- for some t.
--
-- Ps. your function doesn't need to work for inputs 0 and 1, but
-- remember this in the next exercise!

smallestDivisor :: Integer -> Integer
smallestDivisor = undefined

-- Ex 18: implement a function isPrime that checks if the given number
-- is a prime number. Use the function smallestDivisor.
--
-- Ps. 0 and 1 are not prime numbers

isPrime :: Integer -> Bool
isPrime = undefined

-- Ex 19: implement a function nextPrime that returns the first prime
-- number that comes after the given number. Use the function isPrime
-- you just defined.

nextPrime :: Integer -> Integer
nextPrime = undefined
