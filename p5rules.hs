import Data.List.Split (splitOn)

main = do
  contents <- readFile "ペルソナ.tsv"
  let personaList = map makePersona $ lines contents
  mapM putStrLn $ map showPersona personaList

makePersona :: String -> Persona
makePersona lineText =
  let fields = splitOn "\t" lineText
  in Persona (fields !! 0) (fields !! 2)

data Persona = Persona {
  name :: String,
  arcana :: String
} deriving (Eq, Show, Ord)

showPersona :: Persona -> String
showPersona p =
  (name p) ++ " " ++ (arcana p)
