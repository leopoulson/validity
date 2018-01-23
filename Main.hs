data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Show

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
    Not (Not f') -> (Node (f':fs) Leaf Leaf)