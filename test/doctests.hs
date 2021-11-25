module Main where

import Test.DocTest

main :: IO ()
main = do
  putStrLn "Testing comment examples\n"
  putStrLn "Testing Propostions: " *> doctest ["src/Logic/Propositions.hs"]
  putStrLn "Testing Tableaux: " *> doctest ["src/Logic/Tableaux.hs"]
  putStrLn "Testing Normal Forms: " *> doctest ["src/Logic/Normal.hs"]
