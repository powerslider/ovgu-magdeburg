data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show, Read)
type Table a = [a]

update val table = if elem val table then table else table ++ [val]

lookup' elem tableG
    = look elem tableG 0
      where
      look elem table pos 
	  = case table of
             [] -> error ("from lookup': table " ++ show tableG ++ " does not contain elem " ++ show elem)
	     x:xs -> if x == elem then pos else look elem xs (pos + 1)


tree = Node 0
          (Node 1
             (Node 2 Nil Nil)
	     (Node 3 Nil Nil))
	  (Node 4
	     (Node 5 Nil Nil)
	     (Node 6 Nil Nil))

-- Pre-order-Traversierung
-- Hinweis: auf VL-Folien wurde numTree'' mittels case-Konstrukt beschrieben
-- die nachfolgende auf Pattern-Matching basierende Version ist zu dieser aequivalent
numTree'' tree = newTree where
    (newTree, table) = traverseConvert (tree,[])
    traverseConvert (Nil, table) = (Nil, table)
    traverseConvert ((Node val left right), table) = ((Node (lookup' val rightTable) newLeft newRight), rightTable) where
        (newLeft,  leftTable)  = traverseConvert (left, (update val table))
        (newRight, rightTable) = traverseConvert (right, leftTable)

-- *Main> numTree'' tree
-- Node 0 (Node 1 (Node 2 Nil Nil) (Node 3 Nil Nil)) (Node 4 (Node 5 Nil Nil) (Node 6 Nil Nil))


-- In-order-Traversierung
numTreeInO'' :: (Show a, Eq a) => Tree a -> Tree Int
numTreeInO'' tree =  newTree where
    (newTree, table) = traverseConvert (tree,[])
    traverseConvert (Nil, table) = (Nil, table)
    traverseConvert ((Node val left right), table) = ((Node (lookup' val rightTable) newLeft newRight), rightTable) where
        (newLeft, leftTable) = traverseConvert (left, table)
        (newRight, rightTable) = traverseConvert (right, (update val leftTable))

-- *Main> numTreeInO'' tree
-- Node 3 (Node 1 (Node 0 Nil Nil) (Node 2 Nil Nil)) (Node 5 (Node 4 Nil Nil) (Node 6 Nil Nil))


-- Post-order-Traversierung
numTreePostO'' :: (Show a, Eq a) => Tree a -> Tree Int
numTreePostO'' tree = newTree where
    (newTree, table) = traverseConvert (tree,[])
    traverseConvert (Nil, table) = (Nil, table)
    traverseConvert ((Node val left right), table) = ((Node (lookup' val (update val rightTable)) newLeft newRight), (update val rightTable)) where
        (newLeft, leftTable) = traverseConvert (left, table)
        (newRight, rightTable) = traverseConvert (right, leftTable)

-- *Main> numTreePostO'' tree
-- Node 6 (Node 2 (Node 0 Nil Nil) (Node 1 Nil Nil)) (Node 5 (Node 3 Nil Nil) (Node 4 Nil Nil))

