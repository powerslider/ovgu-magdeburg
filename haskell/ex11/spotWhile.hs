import ParseFunctions


spotWhile :: (a -> Bool) -> Parse a [a]
spotWhile f = \s -> [(takeWhile f s, dropWhile f s)]

-- *Main> spotWhile isDigit "234abc"
-- [("234","abc")]
-- *Main> spotWhile isDigit "abc234"
-- [("","abc234")]
