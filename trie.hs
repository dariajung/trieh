module Trie where

import qualified Data.Map as Map
import Control.Monad
import Data.Maybe

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
} deriving (Show)

type Trie = Node (String, Bool)

updateEndWord :: Maybe (String, Bool) -> Maybe (String, Bool)
updateEndWord = liftM update
    where update (v, _) = (v, True)

emptyTrie :: Trie
emptyTrie = Node {
    value = Nothing,
    children = Map.empty
}

initTrie :: t -> Node (t, Bool)
initTrie val = Node {
    value = Just (val, False),
    children = Map.empty
}

insert :: String -> Trie -> Trie
insert [] trie = trie { value = updateEndWord $ value trie }
insert (x:xs) trie = 
    let tc = children trie
        newNode = maybe (initTrie [x]) (initTrie . (++[x]) . fst) (value trie)
        newChildren = Map.insert x newNode tc
    in
    case (Map.lookup x $ children trie) of
        Nothing -> trie { children = Map.insert x (insert xs newNode) newChildren }
        Just trie' -> trie { children = Map.insert x (insert xs trie') tc }

search :: String -> Trie -> Maybe (String, Bool)
search [] trie = value trie
search (x:xs) trie = 
    case (Map.lookup x $ children trie) of
        Nothing -> Nothing
        Just trie' -> search xs trie'
