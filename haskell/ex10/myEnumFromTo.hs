import Test.QuickCheck
import Data.List

myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo n m 
           | n > m = []
           | n == m = [n]
           | otherwise = [n..m]


prop_enum a b c = nub ((myEnumFromTo x y) ++ (myEnumFromTo y z)) == (myEnumFromTo x z) 
                where [x,y,z] = sort [a,b,c]

-- *Main> quickCheck prop_enum
-- (26 tests)  too slow
