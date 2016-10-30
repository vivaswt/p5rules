module Arcana (
  arcanaList,
  combinations
) where

import qualified Sources as SO

arcanaList :: [String]
arcanaList = [
  "愚者", "魔術師", "女教皇", "女帝", "皇帝", "法王", "恋愛", "戦車",
  "正義", "隠者", "運命", "剛毅", "刑死者", "死神", "節制", "悪魔",
  "塔", "星", "月", "太陽", "審判", "世界"]
{-
arcanaList = [
    "愚者", "魔術師", "女教皇", "女帝"]
-}
combinations :: [SO.Sources]
combinations =
  let lists = comb arcanaList 2
  in [(a1, a2) | (a1 : a2 : _) <- lists]
  where comb :: [a] -> Int -> [[a]]
        comb _ 0     = [[]]
        comb [] _     = []
        comb (x:xs) n = map (x:) (comb xs (n-1)) ++ comb xs n
