module Main where

import qualified Data.Map as Map
import Data.List ( intercalate )
import Data.Char ( isDigit )

data Program = JInteger Int | JWord String | JQuotation [Program] | JException String

instance Show Program where
    show (JInteger i) = show i
    show (JWord w) = w
    show (JQuotation ps) = "[ " ++ (unwords . map show) ps ++ " ]"
    show (JException s) = "Exception: " ++ s

data Context = Context { 
    stack :: [Program], 
    queue :: [Program], 
    dict :: Map.Map String Program,
    quotationLevel :: Int }

instance Show Context where
    show c = "s: " ++ show (stack c) ++ "\nq: " ++ show (queue c) ++ "\nd: " ++ show (dict c)

eval :: Context -> Context
eval c | null (queue c) = c
eval c = case p of  JInteger i -> c { stack = p : s, queue = remainingQueue}
                    JWord w -> case Map.lookup w d of
                        Just (JQuotation q) -> c { queue = q ++ remainingQueue }
                        Just def -> c {queue = def : remainingQueue}
                        Nothing -> c { stack = JWord w : s, queue = remainingQueue }
                    JQuotation ps -> c { stack = JQuotation ps : s, queue = remainingQueue }
                    JException e -> c { stack = JException e : s }

    where
        (p : remainingQueue) = queue c
        s = stack c
        d = dict c

pushQueue :: Context -> [Program] -> Context
pushQueue c ps = c { queue = queue c ++ ps }

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
    l <- getLine
    let nc = case l of
                "$step" -> eval c
                cs -> pushQueue c (parse l)
    print nc
    repl nc

main :: IO ()
main = repl $ Context { stack = [], queue = [], dict = Map.fromList [("one", JInteger 1), ("rec", JQuotation [JInteger 1, JWord "rec"])], quotationLevel = 0 }
    