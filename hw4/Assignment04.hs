module Assignment04 where

import FiniteState

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.


fsa_1 :: Automaton Int Bool
fsa_1 = Automaton { startState = 54,
                  finalStates = [38],
                  transitions = [(54, True, 73),
                                 (54, False, 54),
                                 (73, False, 73),
                                 (73, True, 21),
                                 (21, False, 21),
                                 (21, True, 38),
                                 (38, False, 38),
                                 (21, True, 54)] }

fsa_2 :: Automaton Int SegmentCV
fsa_2 = Automaton { startState = 1,
                  finalStates = [3],
                  transitions = [(1, C, 1),
                                 (1, V, 1),
                                 (1, C, 2),
                                 (2, C, 3),
                                 (3, C, 3),
                                 (3, V, 3) ]}
-- recognize fsa_2 [C,C,C,C] => True
-- recognize fsa_2 [V,V,C,V] => False

fsa_3 :: Automaton Int SegmentCV
fsa_3 = Automaton { startState = 1,
                  finalStates = [4],
                  transitions = [(1, V, 2),
                                 (2, V, 1),
                                 (2, C, 3),
                                 (3, C, 2),
                                 (3, V, 4),
                                 (4, V, 3),
                                 (4, C, 1),
                                 (1, C, 4) ]}

fsa_4 :: Automaton Int SegmentCV
fsa_4 = Automaton { startState = 1,
                  finalStates = [4],
                  transitions = [(1, C, 1),
                                 (1, V, 1),
                                 (1, C, 2),
                                 (2, C, 3),
                                 (2, V, 3),
                                 (3, C, 4),
                                 (3, V, 4) ]}

fsa_5 :: Automaton Int SegmentPKIU
fsa_5 = Automaton { startState = 1,
                  finalStates = [3],
                  transitions = [(1, P, 1),
                                 (1, K, 1),
                                 (1, WB, 1),
                                 (1, I, 2),
                                 (2, P, 2),
                                 (2, K, 2),
                                 (2, I, 2),
                                 (2, WB, 1),
                                 (1, P, 3),
                                 (1, K, 3),
                                 (1, WB, 3),
                                 (1, U, 3),
                                 (3, P, 3),
                                 (3, K, 3),
                                 (3, U, 3),
                                 (3, WB, 3)
                                  ]}

fsa_6 :: Automaton Int SegmentPKIU
fsa_6 = Automaton { startState = 1,
                  finalStates = [1, 2],
                  transitions = [(1, K, 1),
                                 (1, I, 1),
                                 (1, P, 2),
                                 (2, K, 2),
                                 (2, P, 2),
                                 (2, I, 2),
                                 (2, U, 2)
                                  ]}
