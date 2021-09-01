module Words where

import Data.List
import qualified Data.Map as Map
import DataStructures
import System.Posix.ByteString (openDirStream)

type BuiltInWord = Context -> Context

openingBracket :: BuiltInWord
openingBracket c | quotationLevel c == 0 = push (JQuotation []) c { quotationLevel = 1 }
openingBracket c = c { quotationLevel = quotationLevel c + 1 }

closingBracket :: BuiltInWord
closingBracket c | quotationLevel c == 0 = push (JException "[] mismatch") c
closingBracket c = c { quotationLevel = quotationLevel c - 1 }

dup :: BuiltInWord
dup c = case uncons s of
            Nothing -> c { stack = [JException "Stack underflow"] }
            Just (p, _) -> c { stack = p : s }
    where
        s = stack c

cons :: BuiltInWord
cons c@Context{stack = JInteger i : JQuotation q : rest} = c { stack = JQuotation (q ++ [JInteger i]) : rest }
cons c@Context{stack = JWord w : JQuotation q : rest} = c { stack = JQuotation (q ++ [JWord w]) : rest }
cons c = push (JException "type errror") c

prelude :: Map.Map String BuiltInWord
prelude = Map.fromList [
    ("[", openingBracket),
    ("]", closingBracket),
    ("dup", dup)
    ]
