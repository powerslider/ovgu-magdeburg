import Data.List

-- "delete x" removes the first occurrence of x from its list argument
-- delete :: Eq a => a -> [a] -> [a]

perms :: (Eq a) => [a] -> [[a]]
perms [] = [[]]
perms l  = [(h:t) | h <- l, t <- perms (delete h l)]


-- Bind/Sequence
permsA :: (Eq a) => [a] -> [[a]]
permsA [] = [[]]
permsA l = l >>= (\h -> permsA (delete h l) >>= (\t -> [h:t]))

-- *Main Data.List> permsA [1,2,3]
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]


-- do-Notation
permsB :: (Eq a) => [a] -> [[a]]
permsB [] = [[]]
permsB l = do h <- l
              t <- permsB (delete h l)
              [h:t]

-- *Main Data.List> permsB [1,2,3]
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
