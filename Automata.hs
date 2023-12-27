-- A deterministic finite automaton 𝒜 is defined by the quintuple 𝒜 = (𝑄, Σ, 𝛿, 𝑞0, 𝐹),
-- where:
-- • 𝑄 is a finite set of states
-- • Σ is a finite set of input symbols (tape alphabet)
-- • 𝛿:𝑄 × Σ → 𝑄 is the transition function
-- • 𝑞0 ∈ 𝑄 is the initial state
-- • 𝐹 ⊆ 𝑄 is the set of final states

-- The file should be structured as follows:
-- A B C <- all states
-- A <- initial state
-- C <- final states
-- 0 1 <- tape symbols
-- A 0 B <- transition function
-- A 1 A
-- B 0 B
-- B 1 C
-- C 0 C
-- C 1 C
module Automata where
type ExtState = ([String], String, [String]) -- state which contains for while 
--                (during conversion or minimization) many states as one state
type State = (String, String, String) -- target state structure 
--                      states symbols stateFunc starting finishing
data Automata = Automata [String] [String] [State] String [String] deriving (Show)
