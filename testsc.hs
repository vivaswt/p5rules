data Persona = Persona {
  name :: String,
  arcana :: String
} deriving (Eq, Show, Ord)

data Example = Example {
  source1 :: String,
  source2 :: String,
  result :: String
} deriving (Eq, Show, Ord)

showPersona :: Persona -> IO ()
showPersona persona = do
  putStrLn $ name persona ++ "[" ++ arcana persona ++ "]"

showExample :: Example -> IO ()
showExample example =
  putStrLn $
    source1 example ++ "x" ++ source2 example
    ++ "->" ++ result example
