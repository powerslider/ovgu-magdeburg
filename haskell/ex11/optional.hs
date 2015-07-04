import ParseFunctions 

optional :: Parse a b -> Parse a [b]
optional a s = let l = (list a s) 
               in [head l] ++ if (length l) > 1 then [head (tail l)] else []

-- *Main> optional (token 'a') "aabb"
-- [("","aabb"),("a","abb")]
