import System.IO
import System.Environment
import Automata
import Minimize
import Convert

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
