module Words where

import qualified Data.Map as Map
import DataStructures

dup :: Program ()
dup = do
    mse <- pop
    case mse of
        Just se -> push se >> push se
        Nothing -> return ()

cons :: Program ()
cons = do
    e <- pop
    q <- pop
    case (e, q) of
        (Just (JInteger i), Just (JQuotation q)) -> push (JQuotation (q ++ [JInteger i]))
        (Just (JWord w), Just (JQuotation q)) -> push (JQuotation (q ++ [JWord w]))
        (_, Nothing) -> return ()
        _ -> push (JException "type error")


prelude :: Map.Map String (Program ())
prelude = Map.fromList [
    ("[", openBracket),
    ("]", closeBracket),
    ("#", define),
    ("dup", dup),
    ("cons", cons)
    ]
