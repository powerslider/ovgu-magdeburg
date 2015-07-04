data ListOfLists a = LL [[a]]

instance (Show a) => Show (ListOfLists a) where
    show (LL xss) = "LL " ++ show xss


instance Functor ListOfLists where
 -- fmap::(a -> b) -> [[a]] -> [[b]]
    fmap = mapLL . mapLL

mapLL _ (ListOfLists []) = (ListOfLists [])
mapLL f ((ListOfLists h): (ListOfLists t)) = (f (ListOfLists h)):(mapLL f (ListOfLists t))
