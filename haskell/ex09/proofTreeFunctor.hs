(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)


instance Functor Tree where
   fmap f (Leaf a) = Leaf (f a)                                      -- fmap.t1
   fmap f (Branch left right) = Branch (fmap f left) (fmap f right)  -- fmap.t2


(1) fmap id    = id
(2) fmap (f.g) = fmap f . (fmap g)

(1) fmap id g (Assume g has the type Tree a)
=> id . g
=> \x -> id (g x)
=> \x -> g x            (*)

* id g
=> g
=> \x -> g x

(2)
-- The inductive hypothesis is:
(fmap (g . f)) Tree x = ((fmap g) . (fmap f)) Tree x

-- LHS:
fmap (g . f) Tree x
=> Branch (fmap (g . f) left) (fmap (g . f) right)                -- fmap defn. [2nd case]

-- RHS:
((fmap g) . (fmap f)) (Tree x)
=> (fmap g) (Branch left right)
=> (fmap g) (Branch (fmap f left) (fmap f right))                 -- fmap defn. [2nd case], on right of .
=> Branch (fmap g (fmap f left)) (fmap g (fmap f right))
=> Branch ((fmap g . fmap f) left) ((fmap g . fmap f) right)      -- definition of composition .
=> Branch (fmap (g . f) left) (fmap (g . f) right)                -- by inductive hypothesis


