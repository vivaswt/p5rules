module Rule (
  Rule (..),
  showRule
) where

data Rule = Rule {
  source1 :: String,
  source2 :: String,
  result :: String
} deriving (Eq, Show, Ord)

showRule :: Rule -> String
showRule rule =
    source1 rule ++ "x" ++ source2 rule
    ++ "->" ++ result rule
