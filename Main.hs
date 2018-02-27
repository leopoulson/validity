import Zipper
import VTree
import Prove
import Parser
import Yoda

prove :: [Form] -> Bool
prove forms = (isAllTrue . checkTree . accumulateTree [] . growZTree . zipifyTree) forms

main :: IO ()
main = do
    putStrLn "Enter the formula to parse: "
    form <- getLine
    print (parseAll formulas form)