import Data.List.Split (splitOn)
import Data.List (find)
import Control.Monad (forM)
import qualified Rule as R
import qualified RuleSummary as RS
import qualified Arcana as AR

main = do
  contents <- readFile "ペルソナ.tsv"
  let (personaList, exampleList) = parse contents
  let ruleList = convertToRule exampleList personaList
  let ruleSummary = RS.summarizeRules ruleList

  {-
  forM ruleSummary $ \rs -> do
    putStrLn $ RS.showRuleSummary rs
   -}

  forM AR.combinations $ \(a1, a2) -> do
    putStrLn $ a1 ++ "," ++ a2

convertToRule :: [Example] -> [Persona] -> [R.Rule]
convertToRule exampleList personaList =
  map (\(Example s1 s2 r) ->
        R.Rule (arcanaOfPersona s1 personaList)
               (arcanaOfPersona s2 personaList)
               (arcanaOfPersona r personaList ))
      exampleList

arcanaOfPersona :: String -> [Persona] -> String
arcanaOfPersona nameToFind personaList =
  maybe "?" arcana result
  where result = find (\p -> name p == nameToFind) personaList

parse :: String -> ([Persona], [Example])
parse contents =
  foldr add ([],[]) $ map parseLine $ tail $ lines contents
  where add (p, es) (ps, ess) = (p : ps, ess ++ es)

parseLine :: String -> (Persona, [Example])
parseLine lineText =
  let fields = splitOn "\t" lineText
      name = fields !! 0
      arcana = fields !! 2
      examplePairs = sourcePairs $ fields !! 5
      examples = foldr (\x acc -> Example (fst x) (snd x) name : acc) [] examplePairs
  in (Persona name arcana, examples)

sourcePairs :: String -> [(String, String)]
sourcePairs text =
  if null text then [] else
    foldr (\x acc -> toPair x : acc) [] $ splitOn ", " text
    where toPair text = (words !! 0, words !! 1)
            where words = splitOn "×" text

-- 指定されたソースの組合せを持つ例を返す
examplesOfPairs :: (String, String) -> [Persona] -> [Example] -> [Example]
examplesOfPairs (s1, s2) personaList exampleList =
  let arcs (Example es1 es2 er) =
        (arcanaOfPersona es1 personaList,
         arcanaOfPersona es2 personaList)
      same (a1, b1) (a2, b2) =
        (a1 == a2 && b1 == b2) || (a1 == b2 && b1 == a2)
      match ex = same (arcs ex) (s1, s2)
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
  source1 :: String,
  source2 :: String,
  result :: String
} deriving (Eq, Show, Ord)
