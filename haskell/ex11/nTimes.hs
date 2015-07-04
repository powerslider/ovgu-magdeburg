import ParseFunctions


nTimes :: Int -> Parse a b -> Parse a [b]
nTimes i p = \s -> let l = (list p) s 
         in if (length l <= i) then [] else [(l !! i)]

-- *Main> nTimes 2 (token 'n') "n"
-- []
-- *Main> nTimes 2 (token 'n') ""
-- []
-- *Main> nTimes 2 (token 'n') "nn"
-- [("nn","")]
-- *Main> nTimes 2 (token 'n') "nnn"
-- [("nn","n")]
-- *Main> 
