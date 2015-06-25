import Test.QuickCheck
import Data.List

-- This function is not correct because it does not alter the list at all
-- it just splits it in half and concatenates it from left to right which
-- results in the original list and not a reversed one.
esrever :: [a] -> [a]
esrever []   = []
esrever [x]  = [x]
esrever list = (esrever left) ++ (esrever right)
               where (left, right) = splitAt (div (length list) 2) list

esreverFixed :: [a] -> [a]
esreverFixed []   = []
esreverFixed [x]  = [x]
esreverFixed list = (esreverFixed right) ++ (esreverFixed left)
               where (left, right) = splitAt (div (length list) 2) list

prop_esreverA list = esrever list == reverse list
                where types = list::String

-- Test fails on first attempt to compare a truly reversed string
-- with the non-reversed one from the esrever function
--
-- *Main> quickCheck prop_esreverA
-- *** Failed! Falsifiable (after 3 tests and 2 shrinks):    
-- "ab"

prop_esreverAFixed list = esreverFixed list == reverse list
                where types = list::String

-- *Main> quickCheck prop_esreverAFixed 
-- +++ OK, passed 100 tests.


prop_esreverB list = length (nub list) == 1 ==> esrever list == reverse list
               where types = list::String

-- *Main> quickCheck prop_esreverB 
-- *** Gave up! Passed only 46 tests.

prop_esreverBFixed list = length (nub list) == 1 ==> esreverFixed list == reverse list
               where types = list::String

-- *Main Data.List Data.Set> quickCheck prop_esreverBFixed 
-- *** Gave up! Passed only 43 tests.

