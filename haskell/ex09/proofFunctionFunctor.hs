(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)


instance Functor ((->) r) where
-- fmap :: (a -> b) -> ((->) r a) -> ((->) r b) 
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)
   fmap f g = \x -> f (g x)


(1) fmap id    = id
(2) fmap (f.g) = (fmap f) . (fmap g)

(1) fmap id g (Assume g has the type (r -> a))
=> id . g
=> \x -> id (g x)
=> \x -> g x            (*)

* id g
=> g
=> \x -> g x

(2) (fmap f . fmap g) h
=> (\x -> fmap f (fmap g x)) h
=> fmap f (fmap g h)
=> fmap f (g . h)
=> f . g . h            (**)

** fmap (f . g) h
=> f . g . h
