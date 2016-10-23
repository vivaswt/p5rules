{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Persona5 as P5

main = do
  contents <- T.readFile "ペルソナ.tsv"
  T.putStrLn $ showLines contents

showLines :: T.Text -> T.Text
showLines = T.unlines . map fields . T.lines

fields :: T.Text -> T.Text
fields lineText =
  let fields = T.splitOn "\t" lineText
  in T.unwords [fields !! 0, fields !! 2, fields !! 5]
