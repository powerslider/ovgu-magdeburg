
newtype State s a = State {runState :: s -> (a,s)}

instance Monad (State s) where
    return x = State (\s -> (x,s))
    (State h) >>= f = State (\s -> let (a, newState) = h s
                                       (State g)     = f a
                                   in  g newState)


rememberConcat :: String -> [Int] -> State String [Int]
rememberConcat s = \l -> State (\x -> (l ++ [length s], x ++ s))

extractRC :: State String [Int] -> ([Int], String)
extractRC (State s) = s ""

undoConcat :: ([Int],String) -> [String]
undoConcat ([], _) = []
undoConcat (x:xs, s) = (take x s) : (undoConcat(xs, drop x s))

-- *Main> undoConcat (extractRC ((return []) >>= rememberConcat "a" >>= rememberConcat "bc" >>= rememberConcat "def"))
-- ["a","bc","def"]
