import Test.QuickCheck

data Studiengang = INF | CV | ING | WIF
                  deriving Show

data Erstsemester = Erstsemester Studiengang
                  deriving Show

instance Arbitrary Erstsemester where
    arbitrary = do studentCount <- choose(0, 100::Int)
                   if studentCount < 18
                   then do return (Erstsemester ING)
                   else if studentCount < 39
                   then do return (Erstsemester WIF)
                   else if studentCount < 54
                   then do return (Erstsemester INF)
                   else do return (Erstsemester CV )

-- *Main> sample (arbitrary :: Gen Erstsemester)
-- Erstsemester CV
-- Erstsemester CV
-- Erstsemester CV
-- Erstsemester ING
-- Erstsemester ING
-- Erstsemester ING
-- Erstsemester WIF
-- Erstsemester INF
-- Erstsemester CV
-- Erstsemester CV
-- Erstsemester ING
