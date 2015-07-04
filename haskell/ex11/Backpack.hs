import Data.List

type Name       = String
type Weight     = Integer
type Item       = (String, Weight)
type ItemList   = [Item]
type CurrWeight = Integer
type MaxWeight  = Integer
type Content    = (ItemList, CurrWeight, MaxWeight)

data Backpack a = Tatters | Backpack a deriving (Show)

backpack = Backpack ([], 0, 100)


instance Monad (Backpack) where
    Tatters >>= k = Tatters
    Backpack x >>= k = k x
    return = Backpack


store :: Item -> Content -> Backpack Content
store (item, weight) (itemList, currWeight, maxWeight) = do 
    if currWeight + weight > maxWeight 
    then Tatters
    else return (((item, weight):itemList), currWeight + weight, maxWeight)


remove :: Item -> Content -> Backpack Content
remove (item, weight) (itemList, currWeight, maxWeight) = do
    if currWeight - weight < 0 
    then Tatters
    else return (delete (item, weight) itemList, currWeight - weight, maxWeight)


pack :: [Content -> Backpack Content] -> Backpack Content -> Backpack Content
pack [] backpack = backpack
pack (x:xs) b = do
    content <- backpack
    pack xs (x content)

-- *Main> backpack >>= store ("Buch", 1) >>= remove ("Buch", 1) >>= store ("Schrank", 100)
-- Backpack ([("Schrank",100)],100,100)
-- *Main> backpack >>= store ("Steinsammlung", 101) >>= remove ("Steinsammlung", 101)
-- Tatters
-- *Main> pack [store ("Buch", 1), remove ("Buch", 1), store ("Schrank", 100)] backpack
-- Backpack ([("Schrank",100)],100,100)
-- *Main>  pack [] backpack
-- Backpack ([],0,100)

