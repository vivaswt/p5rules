module Sources (
  Sources,
  equals,
  source1,
  source2
) where

type Sources = (String, String)

equals :: Sources -> Sources -> Bool
equals (a1, a2) (b1, b2) = (a1 == b1 && a2 == b2) || (a1 == b2 && a2 == b1)

source1 :: Sources -> String
source1 = fst

source2 :: Sources -> String
source2 = snd
