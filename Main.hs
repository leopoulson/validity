data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Show

instance Functor Tree where
    fmap f (Node a t1 t2) = Node (f a) (fmap f t1) (fmap f t2)
    fmap f Leaf = Leaf


data Form = Var Char | Not Form | And Form Form | Or Form Form
    deriving Show



-- validTree :: Tree Bool -> Bool
-- validTree Leaf = True
-- validTree (Node True t1 t2) = (validTree t1) && (validTree t2)
-- validTree (Node False t1 t2) = False

--so the tree keeps track of the rules that it has left to apply

growTree :: Tree [Form] -> Tree [Form]
growTree Leaf = Leaf
growTree (Node (f:fs) Leaf Leaf) = case f of 
    And f1 f2    -> (Node (f1 : f2 : fs) Leaf Leaf)
    Or f1 f2     -> (Node fs (Node (f1:fs) Leaf Leaf) (Node (f2:fs) Leaf Leaf))
    Not (Not f') -> (Node (f':fs) Leaf Leaf)

doubleNeg :: Form -> Form
doubleNeg (Not (Not f)) = f
doubleNeg f = f