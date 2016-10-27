import qualified Rule as R
import qualified RuleSummary as RS

r1 = R.Rule "a1" "a2" "a3"
rs1 = RS.RuleSummary "a1" "a2" ["a4"]
rules = [R.Rule "a1" "a2" "a3"]

main = do
  print $ RS.sameSource r1 rs1
  print $ RS.addResults "a" ["b", "c"]
  print $ RS.addResults "a" ["b", "a"]
