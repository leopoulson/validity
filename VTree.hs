module VTree where

import Zipper

data Tree a = Node a (Tree a) (Tree a) | Leaf 
  deriving Show
type ZTree a = Tree (Zipper a) --ZTree is just used to save time typing

instance Functor Tree where
    fmap f Leaf           = Leaf
    fmap f (Node a bl br) = Node (f a) (fmap f bl) (fmap f br)

data Form = Var Char | Not Form | And Form Form | Or Form Form
    deriving Show
    
zipifyTree :: [Form] -> ZTree Form
zipifyTree [] = Leaf
zipifyTree xs = Node ((fromList) xs) Leaf Leaf

-- This function applies all of the rules to a tree
-- It goes through all of the rules in the Zipper first, 
-- Then when it reaches the Zipper's end it performs the rules on the branches.
growZTree :: ZTree Form -> ZTree Form
growZTree Leaf = Leaf
growZTree (Node z bl br) 
    | endp z    = Node z (growZTree bl) (growZTree br)
    | otherwise = (growZTree . applyZRule) (Node z bl br)

-- So when we apply a rule, we shift the cursor right
-- This lets us retain the old rules, whilst moving
-- onto the next. This is the whole point of using a zipper 
applyZRule :: ZTree Form -> ZTree Form
applyZRule (Node z bl br) = case cursor z of
    Var c                -> Node (right z) bl br
    Not (Var c)          -> Node (right z) bl br

    And f1 f2            -> Node (insert f1 (insert f2 (right z))) bl br
    Not (Or f1 f2)       -> Node (insert (Not f1) (insert (Not f2) (right z))) bl br

    Or f1 f2             -> doubleZRule (Node (right z) bl br) (Or f1 f2)
    Not (And f1 f2)      -> doubleZRule (Node (right z) bl br) (Not (And f1 f2))

    _                    -> Node (right z) bl br
    
    
doubleZRule :: ZTree Form -> Form -> ZTree Form
doubleZRule Leaf _ = Leaf  --This case shouldn't happen 
doubleZRule (Node z Leaf Leaf) f = case f of 
    Or f1 f2        -> Node z (Node (fromList [f1]) Leaf Leaf) (Node (fromList [f2]) Leaf Leaf)
    Not (And f1 f2) -> Node z (Node (fromList [Not (f1)]) Leaf Leaf) (Node (fromList [Not (f2)]) Leaf Leaf)
doubleZRule (Node z bl br) f = Node z (doubleZRule bl f) (doubleZRule br f)

doubleNeg :: Form -> Form
doubleNeg (Not (Not f)) = f
doubleNeg f = f






