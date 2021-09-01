module DataStructures where

import qualified Data.Map as Map

data Program = JInteger Int | JWord String | JQuotation [Program] | JException String
type Dict = Map.Map String Program

showJList :: Show a => [a] -> String
showJList [] = "[]"
showJList xs = "[ " ++ (unwords . map show) xs ++ " ]"

instance Show Program where
    show (JInteger i) = show i
    show (JWord w) = w
    show (JQuotation ps) = showJList ps
    show (JException s) = "Exception: " ++ s

data Context = Context { 
    stack :: [Program], 
    queue :: [Program], 
    dict :: Dict,
    quotationLevel :: Int }

instance Show Context where
    show c = "s: " ++ showJList (stack c) ++ "\nq: " ++ showJList (queue c)


push :: Program -> Context -> Context
push p c = c {stack = p : stack c}

pop :: Context -> (Maybe Program, Context)
pop c | null (stack c) = (Nothing, c {stack = [JException "stack underflow"]})
pop c = (Just p, nc)
    where
        p : remainingStack = stack c
        nc = c { stack = remainingStack }

queuePushFront :: [Program] -> Context -> Context
queuePushFront p c = c { queue = p ++ queue c }

queuePushBack :: [Program] -> Context -> Context
queuePushBack ps c = c { queue = queue c ++ ps }