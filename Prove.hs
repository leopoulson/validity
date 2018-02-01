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

-- So we need to collect all of the rules we get so far, and pass them onto the next one.

traverseZTree :: ZTree Form -> [Form]
traverseZTree = undefined