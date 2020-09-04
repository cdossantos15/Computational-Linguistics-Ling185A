module FiniteState where

-- nub is a simple function for removing duplicates from a list.
import Data.List (nub)

-- Alphabet of {C, V}.
-- NOTE: As I mentioned in class, Show, Eq, and Ord are type-classes,
--       implementing certain functionality for the type; see the handout from
--       3 April.
data SegmentCV = C | V deriving (Show, Eq, Ord)

-- Alphabet of {P, K, I, U, WB}.
data SegmentPKIU = P | K | I | U | WB deriving (Eq, Ord, Show)

-- Model for FSAs using record syntax.
data Automaton states alphabet =
    Automaton { startState :: states,
                finalStates :: [states],
                transitions :: [(states, alphabet, states)] }
    deriving Show

-- Corresponds to (10b) on handout.
-- NOTE: The bit before => forces the type variables s and a to be types that
--       implement the type-class Ord.
targets :: (Ord s, Ord a) => [(s, a, s)] -> s -> a -> [s]
targets [] q x = []
targets (head:tail) q x =
    let (q1, y, q2) = head in -- Unpack the first tuple.
    if q1 == q && y == x then
        q2 : (targets tail q x) -- Save and recurse.
    else
        targets tail q x -- Recurse, but don't save.

-- Corresponds to (10b) on handout (another implementation).
targets' :: (Ord s, Ord a) => [(s, a, s)] -> s -> a -> [s]
targets' delta q x =
    map
        (\(q1, y, q2) -> q2) -- Get the third element.
        (filter (\(q1,y,q2) -> q1 == q && y == x) delta) -- Filter transitions.

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
recognize Automaton {startState=start, finalStates=ends, transitions=delta} u =
    or $ -- Big disjunction: something in list must be True
        map
            (\q -> elem q ends) -- Check if q is an end state.
            (hat delta start u) -- Get all states that would emit u.
