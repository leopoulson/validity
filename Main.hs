import Zipper
import VTree
import Prove

prove :: [Form] -> Bool
prove forms = (isAllTrue . checkTree . accumulateTree [] . growZTree . zipifyTree) forms