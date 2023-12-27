module Convert where
import Automata
import Shared

checkIfNotDeterminization :: [String] -> [String] -> [State] -> Bool
checkIfNotDeterminization [] _ _ = True
checkIfNotDeterminization (x:allStates) symbols stateFunc =
    any (\s -> length (filterStateFuncBySymbol x s stateFunc) > 0) symbols 
    && checkIfNotDeterminization allStates symbols stateFunc

-- gives all set combination from states
stateCombinations :: [String] -> [[String]]
stateCombinations xs = foldl (\acc x -> acc ++ map (++ [x]) acc) [[]] xs

changeStatesFunc :: [String] -> [String] -> [State] -> [ExtState] -> [ExtState]
changeStatesFunc _ [] _ list = list
changeStatesFunc newState (s:symbols) stateFunc list = changeStatesFunc newState symbols stateFunc 
    ((newState, s, (concatMap (\c -> filterStateFuncBySymbol c s stateFunc) newState)):list)

-- go over all combinations
loopOverSymbols :: [String] -> [String] -> [State] -> [ExtState]
loopOverSymbols states symbols stateFunc = concatMap (\c -> changeStatesFunc c symbols stateFunc []) (stateCombinations states)

takeOnlyMatchingStates :: [[String]] -> [ExtState] -> [ExtState] 
takeOnlyMatchingStates [] _ = []
takeOnlyMatchingStates (s:startingState) stateSet = filter (\(x, a, y) -> x == s) stateSet 
    ++ takeOnlyMatchingStates startingState stateSet 

takeInputStates :: [ExtState] -> [[String]]
takeInputStates stateSet = (map (\(x, a, y) -> y) stateSet)

removeSubset :: [ExtState] -> [ExtState] -> [ExtState]
removeSubset originalList subsetToRemove =
  filter (\item -> not (item `elem` subsetToRemove)) originalList

filterUnusedStates :: [[String]] -> [ExtState]-> [ExtState]
filterUnusedStates startingState stateSet
        | (removeSubset stateSet matchingStates) == stateSet = []
        | otherwise = matchingStates ++ filterUnusedStates inputStates (removeSubset stateSet matchingStates)
    where
        matchingStates = takeOnlyMatchingStates startingState stateSet
        inputStates = takeInputStates matchingStates

firstArgMap :: [State] -> [String]
firstArgMap list = map (\(x, a, y) -> x) list

returnNewFinishingStates :: [String] -> [ExtState] -> [String] 
returnNewFinishingStates finishing stateFunc = removeDuplicates $ firstArgMap $ backFormTuple $ filter (\(x, y, z) -> any (`elem` finishing) x) stateFunc

returnNewAllStates :: [ExtState] -> [String]
returnNewAllStates stateFunc = removeDuplicates $ firstArgMap $ backFormTuple stateFunc

convert :: Automata -> Automata
convert (Automata allStates symbols stateFunc starting finishing)
    | checkIfNotDeterminization allStates symbols stateFunc = Automata allStates symbols stateFunc starting finishing
    | otherwise = Automata newStates symbols (backFormTuple newStateFunc) starting newStateFinishing
        where 
            newStateFunc = filterUnusedStates [[starting]] $ loopOverSymbols allStates symbols stateFunc
            newStates= returnNewAllStates newStateFunc
            newStateFinishing = returnNewFinishingStates finishing newStateFunc