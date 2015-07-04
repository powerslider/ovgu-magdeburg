(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)


instance Functor [] where
-- fmap :: (a -> b) -> [a] -> [b]
   fmap _ []    = []                -- fmap.l1
   fmap f (h:t) = (f h):(fmap f t)  -- fmap.l2


(1) fmap id    = id
(2) fmap (f.g) = (fmap f) . (fmap g)

(1) fmap id g (Assume g has the type [a])
=> id . g
=> \x -> id (g x)
=> \x -> g x            (*)

* id g
=> g
=> \x -> g x

(2)
-- The inductive hypothesis is:
(fmap (g . f)) xs = ((fmap g) . (fmap f)) xs

-- LHS:
fmap (g . f) (x:xs)
=> ((g . f) x) : (fmap (g . f) xs)           -- fmap defn. [2nd case]

-- RHS:
((fmap g) . (fmap f)) (x:xs)     
=> (fmap g) ((f x) : (fmap f xs))            -- fmap defn. [2nd case], on right of .
=> (g (f x)) : (fmap g (fmap f xs))          -- fmap defn. [second case]
=> ((g . f) x) : (((fmap g) . (fmap f)) xs)  -- definition of composition .
=> ((g . f) x) : (fmap (g . f) xs)           -- by inductive hypothesis
