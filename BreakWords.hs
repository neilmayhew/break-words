module BreakWords where

import Data.Bifunctor
import Data.Tree
import Data.Trie

breakWords :: Trie -> String -> Forest String
breakWords dict blob =
  [ Node w ws
  | (w, rest) <- splitHeads dict blob
  , let ws = breakWords dict rest
  , null rest || not (null ws)
  ]

splitHeads :: Trie -> String -> [(String, String)]
splitHeads t (c : cs)
  | Just t' <- lookupPrefix [c] t =
      (first (c :) <$> splitHeads t' cs) ++ [([c], cs) | atBreak t']
splitHeads _ _ = []

-- There should be a function that tests for `Trie True _`.
-- We can't do it that way because the constructor isn't exported.
-- Instead, we take advantage of laziness to make this efficient.
atBreak :: Trie -> Bool
atBreak t = take 1 (toList t) == [""]
