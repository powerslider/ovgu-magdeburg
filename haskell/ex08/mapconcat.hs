map' :: (a -> b) -> [a] -> [b]
map' f l = [f x | x <- l]

-- *Main> map' (+1) [1,2,3]
-- [2,3,4]


concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs ]

-- *Main> concat' [[1,2,3],[4,5,6]]
-- [1,2,3,4,5,6]
