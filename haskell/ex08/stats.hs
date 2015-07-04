newtype State s a = State {runState :: s -> (a,s)}

instance Monad (State s) where
    return x = State (\s -> (x,s))
    (State h) >>= f = State (\s -> let (a, newState) = h s
                                       (State g)     = f a
                                   in g newState)


stats item (minimum, maximum, len, sum) = State (\items -> let newMax = maybe item (max item) maximum
                                                               newMin = maybe item (min item) minimum
                                                           in ((Just newMin, Just newMax, len + 1, sum + item), items + [item]))

extractS :: State [a] (Maybe a, Maybe a, Int, Int) -> ([a], Maybe a, Maybe a, Int, Int)
extractS (State trans) = let ((minimum, maximum, len, sum), items) = trans []
                         in (items, minimum, maximum, len, sum)

-- *Main> extractS ((return (Nothing, Nothing, 0, 0)) >>= stats 1 >>= stats 2 >>= stats 0 >>= stats 3)
-- ([1,2,0,3], Just 0, Just 3, 4, 6)
