import Test.QuickCheck

isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = take n list == reverse (drop n list)
                    where n = (div (length list) 2)
-- not correct!!!
-- *Main> isPalindrome "ababa"
-- False


-- This function compares the first half with the second of the list for equality:
-- 
-- 1) if list length is even => simply drop that much elements before
--    getting the second half to be reversed.
-- 2) if list length is odd => drop the length of the first half + 1 before
--    getting the second half to be reversed.
-- 3) when first half and second half are equal => list is palindrome!!!
isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' list = take n list == reverse (drop (n + (len `mod` 2)) list)
	where 
		len = length list
		n = len `div` 2

-- correct !!!
-- *Main> isPalindrome' "ababa"
-- True


-- This property is not suited for valid testing because it
-- always yields correct due to concatenating mirrored strings 
-- which always have an even length.
prop_pal :: [Integer] -> Bool
prop_pal []    = isPalindrome ([]::[Integer])
prop_pal (h:t) = isPalindrome (t ++ reverse t)

-- not correct !!!
-- *Main> quickCheck prop_pal
-- +++ OK, passed 100 tests.


prop_pal' list = length list > 2 ==> isPalindrome' list
            where types = list::[Int]

-- correct error detection !!!
-- *Main> quickCheck prop_pal'
-- *** Failed! Falsifiable (after 1 test and 5 shrinks):
-- [0, 0, 1]


