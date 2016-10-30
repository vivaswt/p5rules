module Rule (
  Rule (..),
  showRule
) where

import qualified Sources as SO

data Rule = Rule {
  sources :: SO.Sources,
  result :: String
} deriving (Eq, Show, Ord)

source1 :: Rule -> String
source1 = SO.source1 . sources

source2 :: Rule -> String
source2 = SO.source2 . sources

showRule :: Rule -> String
showRule rule =
    source1 rule ++ "x"
    ++ source2 rule ++ "->"
    ++ result rule
