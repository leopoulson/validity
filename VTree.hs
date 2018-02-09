module VTree where

-- TODO:
-- Implement something for double negatives
-- Think about what'd happen when you have a branch with one child?
--  ♦︎  This would of course enver happen in reality but still worth keeping in mind


import Zipper

data Tree a = Node a (Tree a) (Tree a) | Leaf 
  deriving Show

type ZTree a = Tree (Zipper a) --ZTree is just used to save time typing

instance Functor Tree where
    fmap f Leaf           = Leaf
    fmap f (Node a bl br) = Node (f a) (fmap f bl) (fmap f br)

data Form = Var Char | Not Form | And Form Form | Or Form Form  
          | Cond Form Form | Bicon Form Form
    deriving (Show, Eq)
    
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
    Not (Cond f1 f2)     -> Node (insert f1 (insert (Not f2) (right z))) bl br

    rule                 -> doubleZRule (Node (right z) bl br) rule

doubleZRule :: ZTree Form -> Form -> ZTree Form
doubleZRule Leaf _ = Leaf  --This case shouldn't happen 
doubleZRule (Node z Leaf Leaf) f = case f of 
    Or a b            -> Node z (Node (fromList [a]) Leaf Leaf) (Node (fromList [b]) Leaf Leaf)
    Cond a b          -> Node z (Node (fromList [Not a]) Leaf Leaf) (Node (fromList [b]) Leaf Leaf)
    Not (And a b)     -> Node z (Node (fromList [Not a]) Leaf Leaf) (Node (fromList [Not b]) Leaf Leaf)

    Bicon a b         -> Node z (Node (fromList [a, b]) Leaf Leaf) (Node (fromList [Not a, Not b]) Leaf Leaf)
    Not (Bicon a b)   -> Node z (Node (fromList [Not a, b]) Leaf Leaf) (Node (fromList [Not b, a]) Leaf Leaf)
doubleZRule (Node z bl br) f = Node z (doubleZRule bl f) (doubleZRule br f)

doubleNeg :: Form -> Form
doubleNeg (Not (Not f)) = f
doubleNeg f = f

accumulateTree :: [Form] -> ZTree Form -> ZTree Form
accumulateTree _ Leaf = Leaf     -- Again, this should never happen
accumulateTree fs (Node z Leaf Leaf) = Node (addAll fs z) Leaf Leaf
accumulateTree fs (Node z bl br) = 
    Node (fromList []) (accumulateTree ((toList z) ++ fs) bl) (accumulateTree ((toList z) ++ fs) br)

-- Adds all of the elements in a list to a zipper
-- TODO: Make it more Haskelly
addAll :: [Form] -> Zipper Form -> Zipper Form
addAll [] z = z
addAll (f:fs) z  = addAll fs (insert f z) 

testAcTree = accumulateTree [Or (Var 'c') (Var 'd')] (Node (fromList [Var 'a']) (Node (fromList [Var 'e']) (Node (fromList [Var 'g']) Leaf Leaf) (Node (fromList [Var 'v']) Leaf Leaf)) (Node (fromList [Var 'b']) Leaf Leaf))




