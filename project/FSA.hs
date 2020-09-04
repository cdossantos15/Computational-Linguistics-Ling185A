module FSA where

-- nub is a simple function for removing duplicates from a list.
import Data.List (nub)

-- Simplified alphabet for Icelandic.
-- NB: TT = "t" (to avoid a naming clash)
data IcelandicCV = A | I | E | O | U | OE | AU | AA
                 | P | TT | K | L | M | D | TH deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- See handout: "Introducing finite-state automata"
-------------------------------------------------------------------------------

-- Corresponds to (4) on handout.
type Automaton state alphabet = (state, [state], [(state, alphabet, state)])

-- Corresponds to (10b) on handout.
targets :: (Ord s, Ord a) => [(s, a, s)] -> s -> a -> [s]
targets [] q x = []
targets (head:tail) q x =
    let (q1, y, q2) = head in -- Unpack the first tuple.
    if q1 == q && y == x then
        q2 : (targets tail q x) -- Save and recurse.
    else
        targets tail q x -- Recurse, but don't save.

-- Corresponds to (11) on handout.
hat :: (Ord s, Ord a) => [(s, a, s)] -> s -> [a] -> [s]
hat delta q [] = [q]
hat delta q (x:u) =
    nub $ concat $ -- Big union and remove duplicates.
        map
            (\q' -> hat delta q' u) -- Recurse on target states.
            (targets delta q x) -- Get possible target states.

-- Corresponds roughly to (13) on handout.
recognize :: (Ord s, Ord a) => Automaton s a -> [a] -> Bool
recognize (start, ends, delta) u =
    or $ -- Big disjunction: something in list must be True
        map
            (\q -> elem q ends) -- Check if q is an end state.
            (hat delta start u) -- Get all states that would emit u.

-- Helpful function to get transitions.
transitions :: (Automaton s a) -> [(s, a, s)]
transitions (_, _, x) = x
