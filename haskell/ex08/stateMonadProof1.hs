newtype State s a = State {runState :: s -> (a,s)}

instance Monad (State s) where
--  return :: a -> State s a
    return x        = State (\s -> (x,s))                   (1)
--  (>>=)  :: State s a -> (a -> State s b) -> State s b
    (State t) >>= f = State (\s -> let (a, newState) = t s
                                       (State h)     = f a
                                   in  (h newState))

-- 1. Apply runState to get a function.
-- 2. Pass in some state to the function.
-- 3. Get back the result and a modified state.

-- (>@>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- f >@> g = \x -> (f x) >>= g             (2)


M1: return x >>= f === f x
M2: m >>= return   === m

-- M1:
-- return >>= f = f x
-- \x -> (return x) >>= f = f x            (2)
-- \x -> (State (\s -> (x,s)) >>= f = f x   (1)
-- f x = f x


-- M2:
-- Definition of bind
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
--
-- Definition of bind for State monad instance
-- (>>=) :: State s a        ->
--          (a -> State s b) ->
--          State s b
--
-- Definition of bind for State monad instance without "State" wrapper
-- (>>=) :: (s -> (a,s))      ->
--          (a -> s -> (b,s)) ->
--          (s -> (b,s))


-- m >>= return = m
-- \x -> (m x) >>= return = m                (2)
-- \x -> (m x) >>= State (\s -> (x,s)) = m   (1)
-- ...
