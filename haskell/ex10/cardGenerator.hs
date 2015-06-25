import Test.QuickCheck
import Control.Monad

data Card = Card Int String deriving (Eq, Show)

instance Arbitrary Card where
    arbitrary = arbitraryVL

arbitraryVL = do int <- arbitrary
                 str <- arbitrary
                 return (Card int str)

-- arbitraryA = frequency[(1, liftM Card arbitrary),
--                       (1, liftM Card arbitrary)]

arbitraryB = do digits <- rand7DigitNum
                upper <- upperAlpha 1
                lower <- lowerAlpha 6
                return (Card digits (upper ++ lower))

-- *Main Test.QuickCheck> sample (arbitraryB :: Gen Card)
-- Card 7231042 "Ovokosn"
-- Card 2089978 "Lydgpxx"
-- Card 3906319 "Oekidlf"
-- Card 2178950 "Ubyebxv"
-- Card 8756624 "Rvmutuc"
-- Card 5226126 "Lsowxyc"
-- Card 3242391 "Hzljjty"
-- Card 3807775 "Iwqnpxg"
-- Card 5747660 "Ltpuycs"
-- Card 7925422 "Jyoolcv"
-- Card 5108495 "Odktdis"


arbitraryC = do digits <- rand7DigitNum
                randNum <- choose(0, 9::Int)
                upper <- upperAlpha 1
                lower <- lowerAlpha randNum
                return (Card digits (upper ++ lower))

-- *Main> sample (arbitraryC :: Gen Card)
-- Card 4203798 "Aybnuv"
-- Card 6567752 "Pz"
-- Card 6795788 "Qu"
-- Card 9988471 "Bhhb"
-- Card 1377740 "Ykqxkc"
-- Card 1284735 "Amolybp"
-- Card 8231374 "S"
-- Card 2556298 "Pxnc"
-- Card 6135193 "Exqjnft"
-- Card 8422036 "E"
-- Card 5873477 "Xil"

rand7DigitNum = elements [1000000..9999999::Int]

upperAlpha n = sequence [elements ['A'..'Z'] | i <- [1..n]]

lowerAlpha n = sequence [elements ['a'..'z'] | i <- [1..n]]
