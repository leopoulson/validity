import Zipper

data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Show

type ZTree a = Tree (Zipper a)

instance Functor Tree where
    fmap f (Node a t1 t2) = Node (f a) (fmap f t1) (fmap f t2)
    fmap f Leaf = Leaf


data Form = Var Char | Not Form | And Form Form | Or Form Form
    deriving Show

--so the tree keeps track of the rules that it has left to apply
growTree :: Tree [Form] -> Tree [Form]
growTree Leaf = Leaf
growTree (Node (f:fs) b1 b2) = case f of 
    And f1 f2       -> (Node (f1 : f2 : fs) b1 b2)
    Not (Or f1 f2)  -> (Node ((Not f1) : (Not f2) : fs) b1 b2)
    Var c           -> (Node [(Var c)] b1 b2)
    Not (Not f')    -> (Node (f':fs) b1 b2)
    _               -> doubleRule (Node (f:fs) b1 b2) f
    -- Or f1 f2        -> (Node fs (growTree (addToTree b1 f1)) (growTree (addToTree b2 f2)))
    -- Not (And f1 f2) -> (Node fs (addToTree b1 (Not f1)) (addToTree b2 (Not f2)))
    
zipifyTree :: [Form] -> ZTree Form
zipifyTree [] = Leaf
zipifyTree xs = Node ((fromList) xs) Leaf Leaf

growZTree :: ZTree Form -> ZTree Form
growZTree Leaf = Leaf
growZTree (Node z bl br) 
    | endp z    = Node z (growZTree bl) (growZTree br)
    | otherwise = (growZTree . applyZRule) (Node z bl br)

applyZRule :: ZTree Form -> ZTree Form
applyZRule (Node z bl br) = case cursor z of
    Var c     -> Node (right z) bl br
    And f1 f2 -> Node (insert f1 (insert f2 (right z))) bl br


--This will add the given Form to the list of Forms in the point at the tree.
addToTree :: Tree [Form] -> Form -> Tree [Form]
addToTree Leaf f = Node [f] Leaf Leaf
addToTree (Node fs t1 t2) f = Node (f:fs) t1 t2

doubleRule :: Tree [Form] -> Form -> Tree [Form]
doubleRule (Node fs Leaf Leaf) f = case f of
    Or f1 f2        -> (Node fs (Node [f1] Leaf Leaf) (Node [f2] Leaf Leaf))
    Not (And f1 f2) -> (Node fs (Node [Not f1] Leaf Leaf) (Node [Not f2] Leaf Leaf))
doubleRule (Node fs b1 b2) f = Node fs (doubleRule b1 f) (doubleRule b2 f)

doubleNeg :: Form -> Form
doubleNeg (Not (Not f)) = f
doubleNeg f = f