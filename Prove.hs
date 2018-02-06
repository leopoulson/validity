module Prove where

import Zipper
import VTree

-- data Tree a = Node a (Tree a) (Tree a) | Leaf 
--   deriving Show
-- type ZTree a = Tree (Zipper a) --ZTree is just used to save time typing

-- So, to prove or disprove a set of rules, we need to go through an
-- entire branch and upon reaching the end consolidate the previous branches, 
-- and check for any duplicate formulas. 

-- NB - A node will only ever have 0 or 2 children. Meaning, when we reach a node that 
-- has to leaves for children, we can assume this to be the end of the branch.
-- We then do the 'proving'.

-- So we need to take a Zipper of Forms, and then get a list of Forms

checkTree :: ZTree Form -> Tree Bool
checkTree t = fmap isValid t

isValid :: Zipper Form -> Bool
isValid z 
  | endp z = True
  | notf (cursor z) `elem` toList z = False
  | otherwise = isValid (right z)

notf :: Form -> Form
notf (Not f) = f
notf f = Not f