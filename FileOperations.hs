module FileOperations where
import System.IO
import Automata

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