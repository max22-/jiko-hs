module DataStructures where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad (when)


data StackElement = JInteger Int | JWord String | JQuotation [StackElement] | JException String
    deriving Eq

type Dict = Map.Map String (Program ())

showJList :: Show a => [a] -> String
showJList [] = "[]"
showJList xs = "[ " ++ (unwords . map show) xs ++ " ]"

instance Show StackElement where
    show (JInteger i) = show i
    show (JWord w) = w
    show (JQuotation ps) = showJList ps
    show (JException s) = "Exception: " ++ s

data Context = Context { 
    stack :: [StackElement], 
    queue :: [StackElement], 
    dict :: Dict,
    quotationLevel :: Int }

instance Show Context where
    show c = "s: " ++ showJList (stack c) ++ "\nq: " ++ showJList (queue c)

type Program a = StateT Context IO a


push :: StackElement -> Program ()
push se = modify (\c -> c {stack = se : stack c})

pop :: Program (Maybe StackElement)
pop = do
    c <- get
    case stack c of
        [] -> put c { stack = [JException "stack underflow"] } >> return Nothing
        tos : rest -> put c { stack = rest } >> return (Just tos)

queuePushFront :: StackElement -> Program ()
queuePushFront se = modify (\c -> c {queue = se : queue c})

queuePushBack :: StackElement -> Program ()
queuePushBack se = modify (\c -> c { queue = queue c ++ [se] })

queuePop :: Program (Maybe StackElement)
queuePop = do
    c <- get
    case queue c of
        [] -> return Nothing
        toq : rest -> put c { queue = rest } >> return (Just toq)

define :: Program ()
define = do
    w <- pop
    definition <- pop
    case (w, definition) of
        (Just (JWord wn), Just (JQuotation q)) -> do
            c <- get
            let nd = Map.insert wn (mapM_ queuePushFront q) (dict c)
            put $ c {dict = nd}
            return ()
        (_, Nothing) -> return ()
        _ -> push (JException "type error")

getDefinition :: String -> Program (Maybe (Program ()))
getDefinition w = do
    c <- get
    case Map.lookup w (dict c) of
        Nothing -> return Nothing
        Just def -> return $ Just def

openBracket :: Program ()
openBracket = do
    c <- get
    when (quotationLevel c == 0) $ push (JQuotation [])
    modify (\c -> c { quotationLevel = quotationLevel c + 1 })

closeBracket :: Program ()
closeBracket = do
    c <- get
    case quotationLevel c of
        n | n <= 0 -> push (JException "[] mismatch")
        n -> modify (\c -> c { quotationLevel = quotationLevel c - 1 })

getQuotationLevel :: Program Int
getQuotationLevel = get >>= (return . quotationLevel)
