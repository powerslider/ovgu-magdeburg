hd :: [a] -> [a]
hd []     =  []
hd (x:xs) =  [x]

tl:: [a] -> [[a]]
tl []     = []
tl (x:xs) = [xs]


transpose :: [[a]] -> [[a]]
transpose []       =  []
transpose ([]:ls)  =  transpose ls
transpose ll       =  [h | (h:_) <- ll] : transpose [t |(_:t) <- ll]


-- Bind/Sequence
transposeA :: [[a]] -> [[a]]
transposeA []       =  []
transposeA ([]:ls)  =  transposeA ls
transposeA ll       =  concat (ll >>= (\l -> return (hd l))) : transposeA (concat (ll >>= (\l -> return (tl l))))

-- *Main> transposeA [[1,2,3], [4,5,6], [7,8]]
-- [[1,4,7],[2,5,8],[3,6]]

-- do-Notation
transposeB :: [[a]] -> [[a]]
transposeB []       =  []
transposeB ([]:ls)  =  transposeB ls
transposeB ll       =  concat (do l <- ll
                                  return (hd l)) : transposeB (concat (do l <- ll
                                                                          return (tl l)))

-- *Main> transposeB [[1,2,3], [4,5,6], [7,8]]
-- [[1,4,7],[2,5,8],[3,6]]
