import Data.List.Split (splitOn)

main = do
  contents <- readFile "ペルソナ.tsv"
  let (personaList, exampleList) = parse contents
  print exampleList

parse :: String -> ([Persona], [Example])
parse contents =
  foldr (\(persona, examples) acc -> (persona : fst acc, examples ++ snd acc)) ([],[]) $ map parseLine $ tail $ lines contents

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

data Persona = Persona {
  name :: String,
  arcana :: String
} deriving (Eq, Show, Ord)

data Example = Example {
  source1 :: String,
  source2 :: String,
  result :: String
} deriving (Eq, Show, Ord)
