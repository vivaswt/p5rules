module Persona5 (
  Persona
) where

{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.Lazy as T

data Persona = Persona {
  name :: T.Text,
  arcana :: T.Text
} deriving (Eq, Show, Ord)
