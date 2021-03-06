module ParseFunctions where


data Expr = Lit Int | Op Ops Expr Expr   deriving (Eq, Show)
data Ops  = Add | Sub | Mul | Div | Mod  deriving (Eq, Show)

isDigit c = c >= '0' && c <= '9'

type Parse a b = [a] -> [(b,[a])]

token :: Eq a => a -> Parse a a 
token t (x:xs)
 | t==x      = [(t,xs)]
 | otherwise = []
token t [] = []

spot :: (a -> Bool) -> Parse a a 
spot p (x:xs)
 | p x       = [(x,xs)]
 | otherwise = []
spot p [] = []

none :: Parse a b
none inp = []

succeed :: b -> Parse a b
succeed val inp = [(val,inp)]

alt :: Parse a b -> Parse a b -> Parse a b
alt p1 p2 inp = p1 inp ++ p2 inp 

infixr 5 >*>
(>*>) :: Parse a b -> Parse a c -> Parse a (b,c)
(>*>) p1 p2 inp = [((y,z),rem2)|(y,rem1) <- p1 inp, (z,rem2) <- p2 rem1]

build  :: Parse a b -> (b -> c) -> Parse a c
build p f inp = [(f x,rem)|(x,rem) <- p inp]

list :: Parse a b -> Parse a [b]
list p = (succeed []) `alt` ((p >*> list p) `build` (uncurry (:)))

listr :: Parse a b -> Parse a [b]
listr p =  ((p >*> listr p) `build` (uncurry (:))) `alt` (succeed [])


