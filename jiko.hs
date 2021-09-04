module Main where

import qualified Data.Map as Map
import Data.List ( intercalate )
import Data.Char ( isDigit )

import DataStructures
import Words
import System.IO ( hFlush, stdout )
import Control.Monad.State ( MonadIO(liftIO), evalStateT, get )

evalStep :: Program ()
evalStep = do
    mse <- queuePop
    case mse of
      Nothing -> return ()
      Just se -> do
          ql <- getQuotationLevel
          case ql of
              0 -> case se of
                JInteger _ -> push se
                JWord "[" -> openBracket
                JWord "]" -> closeBracket
                JWord w -> do
                    mdef <- getDefinition w
                    case mdef of
                        Nothing -> push $ JWord w
                        Just def -> def
                JQuotation ses -> push se
                JException s -> push se
              1 | se == JWord "]" -> closeBracket
              _ -> case se of
                  JInteger _ -> push se >> cons
                  JWord "[" -> openBracket >> push se >> cons
                  JWord "]" -> closeBracket >> push se >> cons
                  JWord _ -> push se >> cons
                  JQuotation _ -> push se >> cons
                  JException _ -> push se >> cons

eval :: Program ()
eval = do
    e <- queueIsEmpty
    if e then return () else {- showContext >> -} evalStep >> eval

isSep :: Char -> Bool
isSep c = c == ' ' || c == '\n'

discardSep :: String -> String
discardSep = dropWhile isSep


takeWord :: String -> String
takeWord = takeWhile (not . isSep ) . discardSep

dropWord :: String -> String
dropWord = dropWhile (not . isSep) . discardSep 

parse1 :: String -> ([StackElement], String)
parse1 xs = ([stackElement], remaining)
    where
        stackElementStr = takeWord xs
        remaining =  dropWord xs
        stackElement = if all isDigit stackElementStr then JInteger (read stackElementStr) else JWord stackElementStr

parse :: String -> [StackElement]
parse "" = []
parse xs = ps ++ parse remaining
    where
        (ps, remaining) = parse1 xs

repl :: Program ()
repl = do
    liftIO $ putStr "> "
    liftIO $ hFlush stdout
    l <- liftIO getLine
    mapM_ queuePushBack (parse l)
    eval
    showStack
    repl

main :: IO ()
main = evalStateT repl $ Context { stack = [], queue = [], dict = prelude, quotationLevel = 0 }