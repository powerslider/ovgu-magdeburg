newtype State s a = State {runState :: s -> (a,s)}

instance Monad (State s) where  
    return x = State (\s -> (x,s))
    (State h) >>= f = State (\s -> let (a, newState) = h s
                                       (State g)     = f a
                                   in  g newState)


joinStrings :: String -> String -> State String String
joinStrings s = \d -> State (\x -> (d, if x == "" then s else x ++ d ++ s))

extractJS :: State String String -> String
extractJS (State s) = snd (s "")

-- *Main> extractJS ((return ";") >>= joinStrings "a" >>= joinStrings "b" >>= joinStrings "c")
-- "a;b;c"

