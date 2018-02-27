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
    let formParsed = (parseAndNegate form)
    if (prove formParsed) then 
        putStrLn "Valid argument!"
    else putStrLn "Invalid argument :("