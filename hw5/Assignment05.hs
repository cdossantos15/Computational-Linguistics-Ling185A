module Assignment05 where

import FiniteState

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

--helper functions

checkPairs :: (Ord sy) => [(sy, sy)] -> (sy, sy) -> Bool
checkPairs [] x = False
checkPairs (hd:tl) x = case (x == hd) of True -> True
                                         False -> checkPairs tl x

isValid :: (Ord sy) => [(sy, sy)] -> [sy] -> Bool
isValid [] x = False
isValid gram [] = True
isValid gram (hd:[]) = True
isValid gram (hd:tl) = case (checkPairs gram (hd, (head tl))) of True -> isValid gram tl
                                                                 False -> False
exStateHelper x = (ExtraState, x, StateForSymbol x)

slgHelper (a, b) = (StateForSymbol a, b, StateForSymbol b)


-- Numero uno.
recognizeSLG :: (Ord sy) => SLG sy -> [sy] -> Bool
recognizeSLG slg [] = False
recognizeSLG ([], y, z) transitions = False
recognizeSLG (hd:tl, y, z) transitions = case ( elem hd transitions) of True -> isValid z transitions
                                                                        False -> recognizeSLG (tl, y, z) transitions


-- Numero dos.
slgToFSA :: SLG sy -> Automaton (ConstructedState sy) sy
slgToFSA (x, y, z) = 
    (start, final, transitions)
    where start = ExtraState
          final = map StateForSymbol y
          transitions = (map exStateHelper x) ++ (map slgHelper z)

-- Numero tres.
unionFSAs :: (Ord sy) => EpsAutomaton Int sy -> EpsAutomaton Int sy
                         -> EpsAutomaton Int sy
unionFSAs = undefined

concatFSAs :: (Ord sy) => EpsAutomaton Int sy -> EpsAutomaton Int sy
                          -> EpsAutomaton Int sy
concatFSAs = undefined

starFSA :: (Ord sy) => EpsAutomaton Int sy -> EpsAutomaton Int sy
starFSA = undefined

reToFSA :: (Ord sy) => RegEx sy -> EpsAutomaton Int sy
reToFSA = undefined
