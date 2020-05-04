import Tokens
import Grammar
import System.Environment
import Control.Exception
import System.IO
import Eval

explodeList :: [a] -> [[a]]
explodeList = map (:[])

combListsXList :: [[a]] -> [a] -> [[a]]
combListsXList [] [] = []
combListsXList [] xs = explodeList xs
combListsXList (x:xs) (y:ys) = (x++[y]) : combListsXList xs ys

combMultipleLists :: [[a]] ->[[a]] -> [[a]]
combMultipleLists [] acc = acc
combMultipleLists [x] acc = combListsXList acc x
combMultipleLists (x:xs) acc = combMultipleLists xs (combListsXList acc x)

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
           let inputTextLines = combMultipleLists (splitWhitespaceAndConvert (lines inputTextWhole)) []
           let tokens = alexScanTokens sourceText
           let parsedProg = parseCalc tokens
           compiledProg <- evalProgram parsedProg inputTextLines
           return ()
        

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()
