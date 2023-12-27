module Shared where
import Automata

filterStateFuncBySymbol :: String -> String -> [State] -> [String]
filterStateFuncBySymbol state symbol stateFunc = map (\(a, b, c) -> c) (filter (\(a, b, c) -> a == state && b == symbol) stateFunc)

backFormTuple :: [ExtState] -> [State]
backFormTuple [] = []
backFormTuple ((x, a, y):rest) = (concat x, a, concat y):(backFormTuple rest)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
