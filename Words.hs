module Words where

import qualified Data.Map as Map
import DataStructures

dup :: Program ()
dup = do
    mse <- pop
    case mse of
        Just se -> push se >> push se
        Nothing -> return ()

swap :: Program ()
swap = do
    mse1 <- pop
    mse2 <- pop
    case (mse1, mse2) of
        (Just se1, Just se2) -> push se1 >> push se2
        _ -> return ()

cons :: Program ()
cons = do
    e <- pop
    q <- pop
    case (e, q) of
        (Just (JInteger i), Just (JQuotation q)) -> push (JQuotation (JInteger i : q))
        (Just (JWord w), Just (JQuotation q)) -> push (JQuotation (JWord w : q))
        (_, Nothing) -> return ()
        _ -> push (JException "type error")

prelude :: Map.Map String (Program ())
prelude = Map.fromList [
    ("[", openBracket),
    ("]", closeBracket),
    ("#", define),
    ("dup", dup),
    ("swap", swap),
    ("cons", cons)
    ]
