module FiniteState where

-- nub is a simple function for removing duplicates from a list.
import Data.List (nub)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----- FINITE STATE AUTOMATA
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Alphabet of {C, V}.
-- NOTE: As I mentioned in class, Show, Eq, and Ord are type-classes,
--       implementing certain functionality for the type; see the handout from
--       3 April.
data SegmentCV = C | V deriving (Show, Eq, Ord)

-- Model for FSAs.
type Automaton states alphabet =
    (states, [states], [(states, alphabet, states)])

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

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----- EPSILON TRANSITIONS
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

type EpsAutomaton st sy = (st, [st], [(st, Maybe sy, st)])

-- Corresponds to (4) on the handout.
efsa_1 :: EpsAutomaton Int Char
efsa_1 = (10, [20,30], [(10, Just 'a', 10), (10, Nothing, 20),
                        (10, Nothing, 30), (20, Just 'b', 21),
                        (21, Just 'b', 20), (30, Just 'b', 31),
                        (31, Just 'b', 32), (32, Just 'b', 30) ])

-- Corresponds to (5) on the handout.
efsa_2 :: EpsAutomaton Int Char
efsa_2 = (0, [2], [(0, Just 'a', 0),
                   (0, Nothing, 1),
                   (1, Just 'b', 1),
                   (1, Nothing, 2),
                   (2, Just 'c', 2)])

-- Corresponds to (6) on the handout.
-- NOTE: 'until p f x' applies f to x repeatedly until p x is true.
epsilonClosure :: (Ord st, Ord sy) => [(st, Maybe sy, st)] -> st -> [st]
epsilonClosure delta q =
    until (\qs -> update qs == qs) update [q]
    where update qs = qs ++ [q2 | q1 <- qs,
                                  q2 <- targets delta q1 Nothing,
                                  not (elem q2 qs)] -- Avoids infinite loop.

-- Helper function getting all the states of a machine.
allStates :: (Ord st) => (st, [st], [(st,a,st)]) -> [st]
allStates (start, ends, delta) =
    nub (start : ends ++ concat [[q1,q2] | (q1,x,q2) <- delta])

-- Helper function getting all the symbols of a machine.
allSymbols :: (Ord a) => (st, [st], [(st,a,st)]) -> [a]
allSymbols (start, ends, delta) = nub [x | (q1, x, q2) <- delta]

-- Corresponds to (7) on the handout.
removeEpsilons :: (Ord st, Ord sy) => EpsAutomaton st sy -> Automaton st sy
removeEpsilons efsa =
    (start, -- Same start state.
     filter canReachEndState (allStates efsa), -- New end states.
     -- NOTE: Apply newTransitions to all the state-symbol combinations, which
     --       will either return [] or a list of transitions; concat then
     --       combines these all into a single list.
     concat [newTransitions q x | q <- allStates efsa, x <- allSymbols efsa])
    where
        (start, ends, delta) = efsa -- Unpack the eFSA.
        newTransitions q1 x = -- Corresponds to (7b) on handout.
            case x of Nothing -> []
                      Just s -> [(q1, s, q3) | q2 <- epsilonClosure delta q1,
                                               q3 <- targets delta q2 (Just s)]
        canReachEndState q =
            or -- Big disjunction.
                (map
                    (\q' -> elem q' ends) -- Is q' an end state?
                    (epsilonClosure delta q)) -- Epsilon closure of q.

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----- STRICTLY-LOCAL GRAMMARS
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Model for strictly-local grammars (sy = symbol).
type SLG sy = ([sy], [sy], [(sy,sy)])
data ConstructedState sy = ExtraState | StateForSymbol sy
                           deriving (Eq, Ord, Show)

-- Example SLGs.
slg1 :: SLG SegmentCV
slg1 = ([C], [V], [(C,C),(C,V),(V,V)])

slg2 :: SLG Int
slg2 = ([1,2,3], [1,2,3], [(1,1),(2,2),(3,3),(1,2),(2,1),(1,3),(3,1)])

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----- REGULAR EXPRESSIONS
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

data RegEx sy = Lit sy
              | Alt (RegEx sy) (RegEx sy)
              | Concat (RegEx sy) (RegEx sy)
              | Star (RegEx sy)
              | Zero
              | One
              deriving Show

re1 :: RegEx Char
re1 = Concat (Alt (Lit 'a') (Lit 'b')) (Lit 'c')

re2 :: RegEx Char
re2 = Star re1

re3 :: RegEx Int
re3 = Star (Concat Zero (Lit 3))

re4 :: RegEx Int
re4 = Concat (Alt (Lit 0) (Lit 1)) (Star (Lit 2))

-- Produces a new version of an FSA with the guarantee that certain integers
-- are not used as state labels.
ensureUnused :: [Int] -> EpsAutomaton Int sy -> EpsAutomaton Int sy
ensureUnused reserved fsa =
    if reserved == [] then
        fsa
    else
        -- nonneg maps integers to nonnegative integers, preserving all
        -- distinctions.
        let nonneg x = if x < 0 then 2 * (-x) - 1 else 2 * x in
        -- Create a version of fsa where all state numbers are nonnegative.
        let fsanonneg = mapStates nonneg fsa in
        -- Add enough to all state numbers to make sure they do not clash with
        -- the reserved list.
        mapStates (\x -> x + 1 + maximum reserved) fsanonneg

-- Adjusts the state labels throughout an FSA.
mapStates :: (a -> b) -> EpsAutomaton a sy -> EpsAutomaton b sy
mapStates f (start, ends, delta) =
    (f start,
     map f ends,
     map
        (\(q1, x, q2) -> (f q1, x, f q2)) -- Apply f to source + target states.
        delta)
