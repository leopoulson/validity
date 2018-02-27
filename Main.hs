import Zipper
import VTree
import Prove
import Parser

prove :: [Form] -> Bool
prove forms = (isAllTrue . checkTree . accumulateTree [] . growZTree . zipifyTree) forms