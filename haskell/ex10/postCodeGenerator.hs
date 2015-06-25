import Test.QuickCheck

data PLZ = PLZ Int deriving Show

--instance Show PLZ where
--    show (PLZ digits) = show (PLZ (take 5 digits))

instance Arbitrary PLZ where
    arbitrary = do postCode <- elements [10000..99999::Int]
                   return (PLZ postCode)

-- *Main> sample (arbitrary :: Gen PLZ)
-- PLZ 22434
-- PLZ 41629
-- PLZ 35851
-- PLZ 91135
-- PLZ 16561
-- PLZ 66444
-- PLZ 53973
-- PLZ 42164
-- PLZ 65412
-- PLZ 27997
-- PLZ 43423

