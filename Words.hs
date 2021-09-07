module Words where

import qualified Data.Map as Map
import DataStructures
import Control.Monad.State ( void, MonadState(put, get), liftIO )

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

drop :: Program ()
drop = void pop

quote :: Program ()
quote = do
    mse <- pop
    case mse of
        Just se -> push (JQuotation [se])
        Nothing -> return ()

cat :: Program ()
cat = do
    mse1 <- pop
    mse2 <- pop
    case (mse1, mse2) of
        (Just (JQuotation q1), Just (JQuotation q2)) -> push $ JQuotation (q2 ++ q1)
        (_, Nothing) -> return ()
        _ -> push $ JException "type error"

app :: Program ()
app = do
    mse <- pop
    case mse of
        Just (JQuotation q) -> mapM_ push q
        Nothing -> return ()
        _ -> push $ JException "type error"

cons :: Program ()
cons = do
    q <- pop
    e <- pop
    case (e, q) of
        (Just (JInteger i), Just (JQuotation q)) -> push (JQuotation (JInteger i : q))
        (Just (JWord w), Just (JQuotation q)) -> push (JQuotation (JWord w : q))
        (_, Nothing) -> return ()
        _ -> push (JException "type error")

append :: Program ()
append = do
    q <- pop
    e <- pop
    case (e, q) of
        (Just (JInteger i), Just (JQuotation q)) -> push (JQuotation (q ++ [JInteger i]))
        (Just (JWord w), Just (JQuotation q)) -> push (JQuotation (q ++ [JWord w]))
        (_, Nothing) -> return ()
        _ -> push (JException "type error")

swons :: Program ()
swons = swap >> cons

clear :: Program ()
clear = get >>= (\c -> put c {stack = [] })

dot :: Program ()
dot = do
    mse <- pop
    case mse of
        Just se -> liftIO . print $ se
        Nothing -> return ()

stack_ :: Program ()
stack_ = do
    c <- get
    push . JQuotation $ stack c



prelude :: Map.Map String (Program ())
prelude = Map.fromList [
    ("[", openBracket),
    ("]", closeBracket),
    ("#", define),
    ("dup", dup),
    ("swap", swap),
    ("drop", Words.drop),
    ("quote", quote),
    ("cat", cat),
    ("i", app),
    ("cons", cons),
    ("swons", swons),
    ("clear", clear),
    (".", dot),
    ("stack", stack_),
    ("append", append)
    ]
