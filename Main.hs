import Zipper
import VTree
import Prove

prove :: [Form] -> Tree Bool
prove forms = (checkTree . growZTree . zipifyTree) forms