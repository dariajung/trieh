module Trie where

import qualified Data.Map as Map

{-- 
    The Trie data type.
    The value can be optional, hence the Maybe data type.
    Children of a Trie are mapped Char to Trie.
    The fullWord value denotes if the Trie holds a value that
    marks the end of a valid word.
--}
data Trie a = Trie {
    value :: Maybe a,
    children :: Map.Map Char (Trie a),
    fullWord :: Bool
}