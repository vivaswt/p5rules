import Data.List (intercalate)

data RuleSummary = RuleSummary {
  source1 :: String,
  source2 :: String,
  results :: [String]
}

showRuleSummary rs =
  (source1 rs) ++ "x" ++ (source2 rs) ++ "->"
  ++ (intercalate ", " . results) rs

showTest =
  intercalate ", " . results
