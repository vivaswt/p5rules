module RuleSummary (
  RuleSummary (..),
  summarizeRules,
  showRuleSummary,
) where

import qualified Rule as R
import Data.List (partition, intercalate)

data RuleSummary = RuleSummary {
  source1 :: String,
  source2 :: String,
  results :: [String]
}


sameSource ::  R.Rule -> RuleSummary -> Bool
sameSource rule summary =
  (R.source1 rule == source1 summary && R.source2 rule == source2 summary)
  || (R.source1 rule == source2 summary && R.source2 rule == source1 summary)

addResults :: String -> [String] -> [String]
addResults result results =
  if result `elem` results then results else result : results

summarizeRules :: [R.Rule] -> [RuleSummary]
summarizeRules rules =
  foldl summarize [] rules
  where summarize summaries rule =
          case same of
            [(RuleSummary s1 s2 r)] ->
              RuleSummary s1 s2 (addResults (R.result rule) r) : others
            [] -> RuleSummary (R.source1 rule) (R.source2 rule) [R.result rule]
              : others
          where (same, others) = partition (sameSource rule) summaries

showRuleSummary :: RuleSummary -> String
showRuleSummary rs =
  source1 rs ++ "x" ++ source2 rs ++ "->"
  ++ intercalate ", " (results rs)
