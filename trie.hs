module Trie where

import qualified Data.Map as Map
import Control.Monad

{- 
    The Node data type.
    
    The value can be optional, hence the Maybe data type.
    Value is also a tuple of the current value and a bool
    to designate if the current value marks the end of a
    word. 

    Children of a Node are mapped Char to Node.
-}
data Node a = Node {
    value :: Maybe a,
    children :: Map.Map Char (Node a)
}

type Trie = Node (String, Bool)

updateEndWord :: Maybe (String, Bool) -> Maybe (String, Bool)
updateEndWord = liftM update
    where update (v, _) = (v, True)

emptyTrie :: Trie
emptyTrie = Node {
    value = Nothing,
    children = Map.empty
}

insert :: String -> Trie -> Trie
insert [] trie = trie { value = updateEndWord $ value trie }
