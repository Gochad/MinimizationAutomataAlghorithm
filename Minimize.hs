module Minimize where
import Automata
import Shared 

makeInitialSets :: [String] -> [String] -> [[String]]
makeInitialSets allStates finishingSets = [filter (\c -> not (c `elem` finishingSets)) allStates, finishingSets]

allPairs :: [String] -> [(String, String)]
allPairs [] = []
allPairs (x:rest) = [(x, y) | y <- rest] ++ allPairs rest

findBySymbol :: String -> String -> [State] -> String
findBySymbol state symbol stateFunc = (map (\(x, a, y) -> y) ((filter (\(x, a, y) -> x == state && a == symbol) stateFunc))) !! 0

findContainingList :: String -> [[String]] -> [String]
findContainingList _ [] = []
findContainingList element (x:rest) 
    | element `elem` x = x
    | otherwise = findContainingList element rest

mapOverSymbolsAndReturnInGroups :: (String, String) -> [String] -> [State] -> [[String]] -> [[[String]]]
mapOverSymbolsAndReturnInGroups _ [] _ _ = []
mapOverSymbolsAndReturnInGroups (a, b) symbols stateFunc sets = 
        [ [ findContainingList (findBySymbol a s stateFunc) sets
            , findContainingList (findBySymbol b s stateFunc) sets
            ]
        | s <- symbols
        ]

simplifySet :: [[[String]]] -> [[String]]
simplifySet [] = []
simplifySet ([a, b]:list) 
    | a == b = a:(simplifySet list)
    | otherwise = []

pairs2InGroups :: [(String, String)] -> [String] -> [State] -> [[String]] -> [([[String]], String, String)]
pairs2InGroups [] _ _ _ = []
pairs2InGroups ((a, b):pairs) symbols stateFunc sets = 
    (((simplifySet $ mapOverSymbolsAndReturnInGroups (a, b) symbols stateFunc sets)), a, b):
    (pairs2InGroups pairs symbols stateFunc sets)

removeNotConnectedElems :: [([[String]], String, String)] -> Int -> [([[String]], String, String)] 
removeNotConnectedElems list symbolsCount = filter (\(sets, a, b) -> length sets == symbolsCount) list

compareElems :: ([[String]], String, String) -> [([[String]], String, String)] -> [String]
compareElems _ [] = []
compareElems (one, a, b) ((two, c, d):list) 
    | (checkListIdentical one two) = [a, b, c, d] ++ (compareElems (one, a, b) list)
    | otherwise = [a, b]
    where 
        checkListIdentical :: [[String]] -> [[String]] -> Bool
        checkListIdentical [] [] = True
        checkListIdentical (x:one) (y:two)
            | x == y = True && (checkListIdentical one two)
            | otherwise = False

mapAndCompare :: [([[String]], String, String)] -> [([[String]], String, String)] -> [[String]]
mapAndCompare [] _ = []
mapAndCompare (x:rest) list = (compareElems x list):(mapAndCompare rest list)

makeSimplierSets :: [[String]] -> [[String]]
makeSimplierSets [] = []
makeSimplierSets (x:list) = (removeDuplicates x):(makeSimplierSets list)

removeDuplicateLists :: (Eq a, Ord a) => [[a]] -> [[a]]
removeDuplicateLists [] = []
removeDuplicateLists (x:xs) = x : removeDuplicateLists (filter (\lst -> sort x /= sort lst) xs)
  where
    sort = foldr insertSorted []
    insertSorted elem [] = [elem]
    insertSorted elem (y:ys)
      | elem <= y = elem : y : ys
      | otherwise = y : insertSorted elem ys

prepareNewSets :: [([[String]], String, String)] -> [[String]]
prepareNewSets toComparison = 
    removeDuplicateLists $ makeSimplierSets $ mapAndCompare toComparison toComparison

checkIfEveryElementHasNewGroup :: [String] -> [[String]] -> [[String]]
checkIfEveryElementHasNewGroup [] newSets = newSets
checkIfEveryElementHasNewGroup (x:set) newSets 
    | (findContainingList x newSets == []) = [x]:(checkIfEveryElementHasNewGroup set newSets)
    | otherwise = (checkIfEveryElementHasNewGroup set newSets)

initNewSets :: [String] -> [String] -> [String] -> [State] -> [String] -> [[String]] -> [[String]]
initNewSets allStates set symbols stateFunc finishing allSets =
    checkIfEveryElementHasNewGroup set $ prepareNewSets (removeNotConnectedElems (pairs2InGroups (allPairs set) symbols stateFunc allSets) (length symbols))

loopOverAllSets :: [[String]] ->[String] -> [String] -> [State] -> [String] ->[[String]] -> [[String]]
loopOverAllSets [] _ _ _ _ _ = []
loopOverAllSets (x:sets) allStates symbols stateFunc finishing allSets
    | (length x == 1) = x:(loopOverAllSets sets allStates symbols stateFunc finishing allSets)
    | otherwise = (initNewSets allStates x symbols stateFunc finishing allSets)++(loopOverAllSets sets allStates symbols stateFunc finishing allSets)

start :: [[String]] -> [String] -> [String] -> [State] -> [String] -> [[String]]
start prevSets allStates symbols stateFunc finishing 
    | prevSets == (loopOverAllSets prevSets allStates symbols stateFunc finishing prevSets) = prevSets
    | otherwise = (start (loopOverAllSets prevSets allStates symbols stateFunc finishing prevSets) allStates symbols stateFunc finishing)

goUntilSetsAreTheSame :: [String] -> [String] -> [State] -> [String] -> [[String]] 
goUntilSetsAreTheSame allStates symbols stateFunc finishing = start (makeInitialSets allStates finishing) allStates symbols stateFunc finishing

makeNewStateFunc :: [[String]] -> [[String]] -> [State] -> [String] -> [[ExtState]]
makeNewStateFunc [] [] _ _ = []
makeNewStateFunc (x:newStates) newStates2 stateFunc symbols = (map (\s -> (x, s, findContainingList ((filterStateFuncBySymbol (x !! 0) s stateFunc) !! 0)newStates2)) symbols):(makeNewStateFunc newStates newStates2 stateFunc symbols)
makeNewStateFunc _ _ _ _ = []

joinNames :: [[String]] -> [String]
joinNames [] = []
joinNames (x:rest) = (concat x):(joinNames rest)

findNewFinishingStates :: [String] -> [[String]] -> [String]
findNewFinishingStates list allStates = removeDuplicates (map (\c -> concat $ findContainingList c allStates) list)

minimize :: Automata -> Automata
minimize (Automata allStates symbols stateFunc starting finishing) = 
    let simplifiedSets = goUntilSetsAreTheSame allStates symbols stateFunc finishing
        newStateFunc = makeNewStateFunc simplifiedSets simplifiedSets stateFunc symbols
    in Automata (joinNames simplifiedSets) symbols (backFormTuple $ concat newStateFunc) starting (findNewFinishingStates finishing simplifiedSets)
   