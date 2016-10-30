import qualified RuleSummary as RS
import Control.Monad (forM)

main = do
  let summaries = [RS.RuleSummary ("愚者", "魔術師") ["太陽"],RS.RuleSummary ("女帝", "魔術師") ["死神"]]

  let result = RS.addNonExampleRule summaries
  forM result $ \rs -> do
    putStrLn $ RS.showRuleSummary rs
