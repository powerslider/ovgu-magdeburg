data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show, Read)
type Table a = [a]

data State a b = State (Table a -> (Table a, b))

instance Monad (State a) where
  return x = State (\tab -> (tab,x))
  (State st) >>= f = State (\tab -> let (newTab,y)    = st tab
                                        (State trans) = f y
                                    in  trans newTab)

lookup' elem tableG
    = look elem tableG 0
      where
      look elem table pos 
	  = case table of
             [] -> error ("from lookup': table " ++ show tableG ++ " does not contain elem " ++ show elem)
	     x:xs -> if x == elem then pos else look elem xs (pos + 1)


numberNode x = State (nNode x)

nNode x table
      | elem x table = (table     , lookup' x table)
      | otherwise    = (table++[x], length table)

extract (State st) = snd (st [])

numTree :: (Show a, Eq a) => Tree a -> Tree Int
numTree = extract . numberTree

numTreeInO :: (Show a, Eq a) => Tree a -> Tree Int
numTreeInO = extract . numberTreeInO

numTreePostO :: (Show a, Eq a) => Tree a -> Tree Int
numTreePostO = extract . numberTreePostO


tree = Node 0
          (Node 1
             (Node 2 Nil Nil)
	     (Node 3 Nil Nil))
	  (Node 4
	     (Node 5 Nil Nil)
	     (Node 6 Nil Nil))


-- Pre-order-Traversierung (vgl. Vorlesung)
numberTree Nil            = return Nil
numberTree (Node x t1 t2) = do num <- numberNode x
                               nt1 <- numberTree t1
                               nt2 <- numberTree t2
                               return (Node num nt1 nt2)

-- In-order-Traversierung
numberTreeInO Nil = return Nil
numberTreeInO (Node x t1 t2) = do nt1 <- numberTree t1
                                  num <- numberNode x
                                  nt2 <- numberTree t2
                                  return (Node num nt1 nt2)

-- Post-order-Traversierung
numberTreePostO Nil = return Nil
numberTreePostO (Node x t1 t2) = do nt1 <- numberTree t1
                                    nt2 <- numberTree t2
                                    num <- numberNode x
                                    return (Node num nt1 nt2)
