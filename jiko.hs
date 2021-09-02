module Main where

import qualified Data.Map as Map
import Data.List ( intercalate )
import Data.Char ( isDigit )

import DataStructures
import Words
import System.IO ( hFlush, stdout )

eval :: Context -> Context
eval c | null (queue c) = c
eval c = case ql of
            0 -> case p of  
                JInteger i -> push p nc
                JWord w -> case Map.lookup w d of
                    Just (JQuotation q) -> queuePushFront q nc
                    Nothing -> case Map.lookup w prelude of
                        Just bw -> bw nc
                        Nothing -> push p nc
                    _ -> push (JException "Invalid definition") nc
                JQuotation ps -> push p nc
                JException e -> push p nc
            n -> case p of
                JWord "]" -> closingBracket nc
                p -> (cons . push p) nc
                    
    where
        (p : remainingQueue) = queue c
        nc = c { queue = remainingQueue }
        s = stack c
        d = dict c
        ql = quotationLevel c

isSep :: Char -> Bool
isSep c = c == ' ' || c == '\n'

discardSep :: String -> String
discardSep = dropWhile isSep


takeWord :: String -> String
takeWord = takeWhile (not . isSep ) . discardSep

dropWord :: String -> String
dropWord = dropWhile (not . isSep) . discardSep 

parse1 :: String -> ([Program], String)
parse1 xs = ([program], remaining)
    where
        programStr = takeWord xs
        remaining =  dropWord xs
        program = if all isDigit programStr then JInteger (read programStr) else JWord programStr

parse :: String -> [Program]
parse "" = []
parse xs = ps ++ parse remaining
    where
        (ps, remaining) = parse1 xs

repl :: Context -> IO ()
repl c = do
    putStr "> "
    hFlush stdout
    l <- getLine
    let nc = case l of
                "$step" -> eval c
                cs -> queuePushBack (parse l) c
    print nc
    repl nc

main :: IO ()
main = repl $ Context { 
    stack = [], 
    queue = [], 
    dict = Map.fromList [("one", JInteger 1), ("rec", JQuotation [JInteger 1, JWord "rec"])],
    quotationLevel = 0 }