-- Deterministyczny automat skończony 𝒜 to piątka 𝒜 = (𝑄, Σ, 𝛿, 𝑞0, 𝐹),
-- gdzie:
-- • 𝑄 jest skończonym zbiorem stanów
-- • Σ jest skończonym zbiorem symboli wejściowych (alfabet taśmy)
-- • 𝛿:𝑄 × Σ → 𝑄 jest funkcją przejścia
-- • 𝑞0 ∈ 𝑄 jest stanem początkowym
-- • 𝐹 ⊆ 𝑄 jest zbiorem stanów końcowych

-- plik ma być zbudowany tak:
-- A B C <- wszystkie stany
-- A <- stan początkowy
-- C <- stany końcowe
-- 0 1 <- symbole taśmy
-- A 0 B <- funkcja przejść
-- A 1 A
-- B 0 B
-- B 1 C
-- C 0 C
-- C 1 C
import System.IO
import System.Environment

type ExtState = ([String], String, [String])
type State = (String, String, String)
--                      states symbols stateFunc starting finishing
data Automata = Automata [String] [String] [State] String [String] deriving (Show)

-- READ AUTOMA FROM FILE
parseLine :: String -> State
parseLine line = (\[a, b, c] -> (a, b, c)) (words line)

readStatesTable :: Handle -> IO [State]
readStatesTable handle = do
  eof <- hIsEOF handle
  if eof
    then return []
    else do
      line <-hGetLine handle
      rest <-readStatesTable handle
      return (parseLine line : rest)


file2automaton :: Handle -> IO Automata
file2automaton handle = do
    allStates<-hGetLine handle
    starting<-hGetLine handle
    finishing<-hGetLine handle
    symbols<-hGetLine handle
    statesFunc<-readStatesTable handle
    return (Automata (words allStates) (words symbols) statesFunc starting (words finishing))

-- SAVE DFA TO FILE
saveLine :: Handle -> [String] -> IO ()
saveLine handle [] = do
    hPutStr handle "\n"
    return ()
saveLine handle (x:xs) = do
   hPutStr handle (x ++ " ")
   saveLine handle xs

saveStates :: Handle -> [State] -> IO ()
saveStates handle [] = return ()
saveStates handle ((a, b, c):states) = do
    hPutStrLn handle (a ++ " " ++ b ++ " " ++ c)
    saveStates handle states

dfa2file :: Handle -> Automata -> IO ()
dfa2file handle (Automata allStates symbols statesFunc starting finishing) = do
    saveLine handle allStates
    hPutStrLn handle starting
    saveLine handle finishing
    saveLine handle symbols
    saveStates handle statesFunc

-- NFA TO DFA
checkIfNotDeterminization :: [String] -> [String] -> [State] -> Bool
checkIfNotDeterminization [] _ _ = True
checkIfNotDeterminization (x:allStates) symbols stateFunc =
    any (\s -> length (filterStateFuncBySymbol x s stateFunc) > 0) symbols 
    && checkIfNotDeterminization allStates symbols stateFunc

-- gives all set combination from states
stateCombinations :: [String] -> [[String]]
stateCombinations xs = foldl (\acc x -> acc ++ map (++ [x]) acc) [[]] xs

filterStateFuncBySymbol :: String -> String -> [State] -> [String]
filterStateFuncBySymbol state symbol stateFunc = map (\(a, b, c) -> c) (filter (\(a, b, c) -> a == state && b == symbol) stateFunc)

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

-- to generowanie nowych stanów nie dziala dobrze
-- input: filterUnusedStates [["q0"],["q0","q1"]] [([],"1",[]),([],"0",[]),(["q0"],"1",["q0"]),(["q0"],"0",["q0","q1"]),(["q1"],"1",[]),(["q1"],"0",["q2"]),(["q0","q1"],"1",["q0"]),(["q0","q1"],"0",["q0","q1","q2"]),(["q2"],"1",[]),(["q2"],"0",[]),(["q0","q2"],"1",["q0"]),(["q0","q2"],"0",["q0","q1"]),(["q1","q2"],"1",[]),(["q1","q2"],"0",["q2"]),(["q0","q1","q2"],"1",["q0"]),(["q0","q1","q2"],"0",["q0","q1","q2"])]
-- output: [(["q0"],"1",["q0"]),(["q0"],"0",["q0","q1"]),(["q0","q1"],"1",["q0"]),(["q0","q1"],"0",["q0","q1","q2"]),(["q0","q1","q2"],"1",["q0"]),(["q0","q1","q2"],"0",["q0","q1","q2"])]
-- (["q0","q1"],"0",["q0","q1","q2"])
filterUnusedStates :: [[String]] -> [ExtState]-> [ExtState]
filterUnusedStates startingState stateSet
        | (removeSubset stateSet matchingStates) == stateSet = []
        | otherwise = matchingStates ++ filterUnusedStates inputStates (removeSubset stateSet matchingStates)
    where
        matchingStates = takeOnlyMatchingStates startingState stateSet
        inputStates = takeInputStates matchingStates

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

firstArgMap :: [State] -> [String]
firstArgMap list = map (\(x, a, y) -> x) list

returnNewFinishingStates :: [String] -> [ExtState] -> [String] 
returnNewFinishingStates finishing stateFunc = removeDuplicates (firstArgMap (backFormTuple (filter (\(x, y, z) -> any (`elem` finishing) x) stateFunc)))

returnNewAllStates :: [ExtState] -> [String]
returnNewAllStates stateFunc = removeDuplicates (firstArgMap (backFormTuple stateFunc))

backFormTuple :: [ExtState] -> [State]
backFormTuple [] = []
backFormTuple ((x, a, y):rest) = (concat x, a, concat y):(backFormTuple rest)

xd :: Automata -> [ExtState]
xd (Automata allStates symbols stateFunc starting finishing) = (loopOverSymbols allStates symbols stateFunc)

convert :: Automata -> Automata
convert (Automata allStates symbols stateFunc starting finishing)
    | (checkIfNotDeterminization allStates symbols stateFunc) == True = Automata allStates symbols stateFunc starting finishing
    | otherwise = Automata (returnNewAllStates newStateFuncExtended) symbols (backFormTuple newStateFuncExtended) starting (returnNewFinishingStates finishing newStateFuncExtended)
        where 
            newStateFuncExtended = filterUnusedStates [[starting]] (loopOverSymbols allStates symbols stateFunc)

-- returnNewAllStates (filterUnusedStates [["q0"]] (loopOverSymbols ["q0","q1","q2"] ["0","1"] [("q0","0","q0"),("q0","0","q1"),("q0","1","q0"),("q1","1","q2")]))
-- filterUnusedStates [["A"]] (loop ["A", "B", "C"] ["0", "1"] [("A", "0", "A"), ("A", "0", "B"), ("A", "1", "A"), ("B", "1", "C")]) []
-- [(["A"],"1",["A"]),(["A"],"0",["A","B"]),(["A","B"],"1",["A","C"]),(["A","B"],"0",["A","B"]),(["A","C"],"1",["A"]),(["A","C"],"0",["A","B"])]

-- DFA MINIMIZATION ALGORITHM
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
    (((simplifySet(mapOverSymbolsAndReturnInGroups (a, b) symbols stateFunc sets)), a, b)):
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
    removeDuplicateLists (makeSimplierSets (mapAndCompare toComparison toComparison))

checkIfEveryElementHasNewGroup :: [String] -> [[String]] -> [[String]]
checkIfEveryElementHasNewGroup [] newSets = newSets
checkIfEveryElementHasNewGroup (x:set) newSets 
    | (findContainingList x newSets == []) = [x]:(checkIfEveryElementHasNewGroup set newSets)
    | otherwise = (checkIfEveryElementHasNewGroup set newSets)

initNewSets :: [String] -> [String] -> [String] -> [State] -> [String] -> [[String]] -> [[String]]
initNewSets allStates set symbols stateFunc finishing allSets =
    checkIfEveryElementHasNewGroup set (prepareNewSets (removeNotConnectedElems (pairs2InGroups (allPairs set) symbols stateFunc allSets) (length symbols)))

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
findNewFinishingStates list allStates = removeDuplicates (map (\c -> concat (findContainingList c allStates)) list)

minimize :: Automata -> Automata
minimize (Automata allStates symbols stateFunc starting finishing) = 
    let simplifiedSets = goUntilSetsAreTheSame allStates symbols stateFunc finishing
        newStateFunc = makeNewStateFunc simplifiedSets simplifiedSets stateFunc symbols
    in Automata (joinNames simplifiedSets) symbols (backFormTuple (concat newStateFunc)) starting (findNewFinishingStates finishing simplifiedSets)
    
main :: IO ()
main = do
    (readAutomataFile:writeAutomataFile:_) <- getArgs
    readFileHandle <- openFile readAutomataFile ReadMode
    writeFileHandle <- openFile writeAutomataFile WriteMode
    automata <-file2automaton readFileHandle
    putStrLn (show(automata) ++ "\n" ++ (show ((convert automata))))
    dfa2file writeFileHandle (minimize (convert automata))
    hClose readFileHandle
    hClose writeFileHandle


