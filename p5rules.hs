import Data.List.Split (splitOn)
import Data.List (find)
import Control.Monad (forM)
import qualified Rule as R
import qualified RuleSummary as RS
import qualified Sources as SO

main = do
  contents <- readFile "ペルソナ.tsv"
  let (personaList, exampleList) = parse contents
  let ruleList = convertToRule exampleList personaList
  let ruleSummary = RS.summarizeRules ruleList

  forM ruleSummary $ \rs -> do
    putStrLn $ RS.showRuleSummary rs

   {-
  forM AR.combinations $ \(a1, a2) -> do
    putStrLn $ a1 ++ "," ++ a2
    -}

convertToRule :: [Example] -> [Persona] -> [R.Rule]
convertToRule exampleList personaList =
  map (\(Example (s1, s2) r) ->
        R.Rule (arcanaOfPersona s1 personaList,
                arcanaOfPersona s2 personaList)
               (arcanaOfPersona r personaList ))
      exampleList

arcanaOfPersona :: String -> [Persona] -> String
arcanaOfPersona nameToFind personaList =
  maybe "?" arcana result
  where result = find (\p -> name p == nameToFind) personaList

parse :: String -> ([Persona], [Example])
parse contents =
  foldr add ([],[]) $ map parseLine . tail . lines $ contents
  where add (p, es) (ps, ess) = (p : ps, ess ++ es)

parseLine :: String -> (Persona, [Example])
parseLine lineText =
  let fields = splitOn "\t" lineText
      name = fields !! 0
      arcana = fields !! 2
      examplePairs = sourcePairs (fields !! 5)
      examples = foldr (\x acc -> Example x name : acc) [] examplePairs
  in (Persona name arcana, examples)

sourcePairs :: String -> [SO.Sources]
sourcePairs [] = []
sourcePairs text =
  let pairs = splitOn ", " text
      words = map (splitOn "×") pairs
      result = map (\(s1 : s2 : _) -> (s1, s2)) words
  in result

-- 指定されたソースの組合せを持つ例を返す
examplesOfPairs :: (String, String) -> [Persona] -> [Example] -> [Example]
examplesOfPairs (s1, s2) personaList exampleList =
  let arcs (Example (es1, es2) _) =
        (arcanaOfPersona es1 personaList,
         arcanaOfPersona es2 personaList)
      match = SO.equals (s1, s2) . arcs
      result = filter match exampleList
  in result

showPersona :: Persona -> String
showPersona persona =
  name persona ++ "[" ++ arcana persona ++ "]"

showExample :: Example -> String
showExample example =
    source1 example ++ "x" ++ source2 example
    ++ "->" ++ result example

data Persona = Persona {
  name :: String,
  arcana :: String
} deriving (Eq, Show, Ord)

data Example = Example {
  sources :: SO.Sources,
  result :: String
} deriving (Eq, Show, Ord)

source1 :: Example -> String
source1 = SO.source1 . sources

source2 :: Example -> String
source2 = SO.source2 . sources
