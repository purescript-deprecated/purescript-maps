-- | This module defines a type of maps as balanced 2-3 trees, based on
-- | <http://www.cs.princeton.edu/~dpw/courses/cos326-12/ass/2-3-trees.pdf>

module Data.Map
  ( module Data.Map.Internal
  ) where

import Data.Map.Internal
  ( Map
  , showTree
  , empty
  , isEmpty
  , singleton
  , checkValid
  , insert
  , lookup
  , lookupLE
  , lookupLT
  , lookupGE
  , lookupGT
  , findMin
  , findMax
  , fromFoldable
  , fromFoldableWith
  , toUnfoldable
  , toAscUnfoldable
  , delete
  , pop
  , member
  , alter
  , update
  , keys
  , values
  , union
  , unionWith
  , unions
  , isSubmap
  , size
  , mapWithKey
  , filterWithKey
  , filterKeys
  , filter
  )
