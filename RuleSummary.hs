module RuleSummary (
  RuleSummary (..),
  summarizeRules,
  showRuleSummary
) where

import qualified Rule as R
import qualified Sources as SO
import Data.List (partition, intercalate)

data RuleSummary = RuleSummary {
  sources :: SO.Sources,
  results :: [String]
}

source1 :: RuleSummary -> String
source1 = SO.source1 . sources

source2 :: RuleSummary -> String
source2 = SO.source2 . sources

sameSource ::  R.Rule -> RuleSummary -> Bool
sameSource rule summary =
  SO.equals (R.sources rule) (sources summary)

addResults :: String -> [String] -> [String]
addResults result results =
  if result `elem` results then results else result : results

summarizeRules :: [R.Rule] -> [RuleSummary]
summarizeRules rules =
  foldl summarize [] rules
  where summarize summaries rule =
          case same of
            [(RuleSummary s r)] ->
              RuleSummary s (addResults (R.result rule) r) : others
            [] -> RuleSummary (R.sources rule) [R.result rule]
              : others
          where (same, others) = partition (sameSource rule) summaries

showRuleSummary :: RuleSummary -> String
showRuleSummary rs =
  source1 rs ++ "x" ++ source2 rs ++ "->"
  ++ intercalate ", " (results rs)
