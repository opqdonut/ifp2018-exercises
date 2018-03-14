Introduction to Functional Programming, exercises
=================================================

Getting started
---------------

1. Install [Stack](https://www.haskellstack.org/)

2. Open a terminal

3. Clone this repository by running

        git clone https://github.com/opqdonut/ifp2018-exercises.git

4. Change directory to the cloned repository

        cd ifp2018-exercises

5. Download dependencies by running

        stack build

6. Then check you can actually run the tests with:

        stack runhaskell W0Test.hs

    This should print `Everything seems to be OK!`. If you see any errors,
    you might have a problem with your Haskell installation.

7. Now you can edit `W1.hs` and see how well you did by running

        stack runhaskell W1Test.hs

Working on the exercises
------------------------

- Edit the Wn.hs files according to the instructions
- Don't remove or change any type signatures (things like `foo ::
  String -> String`) that are already in the files
- Check your answers for week `n` with `stack runhaskell WnTest.hs`
  (or just `runhaskell WnTest.hs` if you're not using stack)
- A typical test failure looks like this:

        Testing 11
        *** Failed! Falsifiable (after 1 test):
        0
        0
        Expected 1, got 3
        FAIL

    This means that the function from exercise 11 failed the test when
    the two arguments were 0 and 0. The result should have been 1, but
    it was 3.

    I'm sorry if the test failures aren't always understandable :/

Using GHCi
----------

- You can also play around with your solutions interactively by
  running `stack exec ghci Wn.hs` (or `ghci Wn.hs`).
  This is a good idea for instance when you don't understand the
  test failures.
- Use the `:reload` command to reload the file if you've made
  changes.
- You can use the `:type` command to get the type of an expression, for example
  `:type tail "asdf"`
