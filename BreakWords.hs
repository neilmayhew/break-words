module BreakWords where

import Data.List (tails)
import Data.Trie

import qualified Data.Set as Set

type Node = Int
type Edge = Int

breakWords :: Trie -> String -> [(Node, [Edge])]
breakWords dict blob =
    [ ( node,
        [ edge
        | edge <- edges !! node
        , (node + edge) `Set.member` head nodes
        ]
      )
    | node <- Set.toList $ head nodes
    ]
  where
    l = length blob
    edges = map (splitHeads dict) (tails blob)
    nodes = map go [0 .. l]
    go n =
       let subnodes = Set.unions [nodes !! (n + edge) | edge <- edges !! n]
       in if n == l || not (null subnodes)
            then Set.insert n subnodes
            else subnodes

splitHeads :: Trie -> String -> [Int]
splitHeads t (c : cs)
  | Just t' <- lookupPrefix [c] t =
      [1 | atBreak t'] ++ (succ <$> splitHeads t' cs)
splitHeads _ _ = []

-- There should be a function that tests for `Trie True _`.
-- We can't do it that way because the constructor isn't exported.
-- Instead, we take advantage of laziness to make this efficient.
atBreak :: Trie -> Bool
atBreak t = take 1 (toList t) == [""]
