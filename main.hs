import System.IO
import System.Environment
import Automata
import Minimize
import Convert
import FileOperations
 
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
