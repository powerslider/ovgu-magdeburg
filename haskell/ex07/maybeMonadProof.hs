-- instance Monad Maybe where
--     Just x >>= k = k x      (1)
--     Nothing >>= _ = Nothing (2)
-- 
--     Just _ >> k = k
--     Nothing >> _ = Nothing
-- 
--     return x = Just x       (3)
--     fail _ = Nothing
-- 
--  M1: return >@> f = f
--  M2: f >@> return = f
--  M3: (f >@> g) >@> h = f >@> (g >@> h)
-- 
-- 
-- (>@>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- f >@> g = \x -> (f x) >>= g      (4)
-- 
-- 
-- M1:
-- return >@> f = f
-- \x -> (return x) >>= f = f      (4)
-- \x -> (Just x) >>= f = f        (3)
-- Just >>= f = f
-- Just x >>= f = f x              (1)
-- f x = f x
-- 
-- M2:
-- f >@> return = f
-- \x -> (f x) >>= return = f      (4)
-- \x -> (f x) >>= Just = f        (3)
-- f >>= Just = f
-- f x >>= Just = f x
-- 
-- 
-- M3:
-- (f >@> g) >@> h = f >@> (g >@> h)
-- \x -> ((f >@> g) x) >>= h = f >@> (g >@> h)             (4)
-- \x -> ((\y -> (f y) >>= g) x) >>= h = f >@> (g >@> h)   (4)
-- ((\y -> (f y) >>= g) >>= h = f >@> (g >@> h)
-- (f >>= g) >>= h = f >@> (g >@> h)
-- (f x >>= g) >>= h = (f >@> (g >@> h)) x
-- 
