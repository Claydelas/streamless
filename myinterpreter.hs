import Tokens
import Grammar
import System.Environment
import Control.Exception
import System.IO
import Eval

explodeList :: [a] -> [[a]]
explodeList = map (:[])

combineListsXList :: [[a]] -> [a] -> [[a]]
combineListsXList [] [] = []
combineListsXList [] xs = explodeList xs
combineListsXList (x:xs) (y:ys) = (x++[y]) : combineListsXList xs ys

combineMultipleLists :: [[a]] ->[[a]] -> [[a]]
combineMultipleLists [] acc = acc
combineMultipleLists [x] acc = combineListsXList acc x
combineMultipleLists (x:xs) acc = combineMultipleLists xs (combineListsXList acc x)

splitWhitespaceAndConvert :: [String] -> [[Int]]
splitWhitespaceAndConvert = map (convertToInt . words)

convertToInt :: [String] -> [Int]
convertToInt = map (\ x -> read x :: Int)

main :: IO ()
main = catch main' noParse

main' :: IO ()
main' = do (fileName: _) <- getArgs
           sourceText <- readFile fileName
           inputTextWhole <- getContents
           let inputTextLines = combineMultipleLists (splitWhitespaceAndConvert (lines inputTextWhole)) []
           let tokens = alexScanTokens sourceText
           let parsedProg = parseCalc (alexScanTokens sourceText)
           compiledProg <- evaluateProgram parsedProg inputTextLines
           return ()
        

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()