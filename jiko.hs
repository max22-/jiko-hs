module Main where

data Program = JInteger Int | JWord String | Quotation [Program] | JException String
    deriving Show

isSep :: Char -> Bool
isSep c = c == ' ' || c == '\n'

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

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

repl :: IO ()
repl = do
    putStr "> "
    l <- getLine
    print . parse $ l
    repl

main :: IO ()
main = repl
    