module Final where

-- Relevant data structures.
import FSA
import CFG
import Parsing

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line. :-)
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
 --CODE FOR DEBUGGING--
import Control.Monad (filterM)
import Data.List (transpose)
import Text.PrettyPrint.Boxes

-- Used for generating rules with optionality.
powerset :: [a] -> [[a]]
powerset xs = filterM (const [True, False]) xs

-- Grammar used in class.
cfg1 :: CFG String String
cfg1 = ( "S", [ NonterminalRule "S" ["NP", "VP"]
              , NonterminalRule "S" ["WHILE", "S", "S"]
              , NonterminalRule "NP" ["NP", "POSS", "N"]
              , NonterminalRule "PP" ["P", "NP"]
              , NonterminalRule "SRC" ["THAT", "VP"]
              , NonterminalRule "ORC" ["NP", "V"]
              , TerminalRule "N" "friend"
              , TerminalRule "N" "neighbor"
              , TerminalRule "N" "child"
              , TerminalRule "N" "actor"
              , TerminalRule "N" "baby"
              , TerminalRule "N" "boy"
              , TerminalRule "NP" "Alex"
              , TerminalRule "V" "met"
              , TerminalRule "V" "saw"
              , TerminalRule "V" "won"
              , TerminalRule "D" "the"
              , TerminalRule "P" "on"
              , TerminalRule "P" "in"
              , TerminalRule "P" "with"
              , TerminalRule "THAT" "that"
              , TerminalRule "POSS" "'s"
              , TerminalRule "WHILE" "while"
              ]
              ++ map (\xs -> NonterminalRule "NP" ("N" : xs))
                     (powerset ["PP", "SRC", "ORC"])
              ++ map (\xs -> NonterminalRule "NP" ("D" : "N" : xs))
                     (powerset ["PP", "SRC", "ORC"])
              ++ map (\xs -> NonterminalRule "VP" ("V" : xs))
                     (powerset ["NP", "PP"])
              )

printParseList parses = do
    mapM_ printParse parses

-- Prints a parse as a nice table.
printParse parse = do
    putStrLn "===== BEGIN PARSE ====="
    let parse' = map (\(a, b, c) -> [show a, showRule b, show c])
                     (reverse parse)
    printBox $ hsep 2 left (map (vcat left . map text) (transpose parse'))
    putStrLn "===== END PARSE ======="
  where
    showRule (TerminalRule l r)    = show l ++ " -> " ++ show r
    showRule (NonterminalRule l r) = show l ++ " -> " ++ show r
    showRule NoRule                = "NoRule"

--TESTING HELP
cfg2 :: CFG String String
cfg2 = ( "S"
       , [ NonterminalRule "S" ["NP", "VP"]
         , NonterminalRule "NP" ["D", "N"]
         , NonterminalRule "VP" ["V", "NP"]
         , TerminalRule "NP" "John"
         , TerminalRule "NP" "Mary"
         , TerminalRule "D" "the"
         , TerminalRule "D" "a"
         , TerminalRule "N" "cat"
         , TerminalRule "N" "dog"
         , TerminalRule "V" "saw"
         , TerminalRule "V" "likes"
         ]
       )

-------------------------------------------------------------------------------
-- Question 1:
-------------------------------------------------------------------------------

{-
*Any A before U becomes OE*
State 1 loop: Accepts everything but U and A
State 2 loop: Accepts everything but A
State 3 loop: Accepts everything but U
Transition 1-2: U
Transition 1-3: A
Transition 2-3: A
If it transitions from 1 to state 2 through 'U', then 'A' no longer appears in the string
tests: recognize icelandic [TH, A, L, A] => True
    			 icelandic [TH, OE, L, U] => True
    			 icelandic [E, I, O, OE, AA, M, D, U] => True
    			 icelandic [E, I, O, OE, AA, M, D, U, A, D] => True
    			 icelandic [TH, A, L, U, M] => False
    			 icelandic [E, I, O, OE, AA, TT, M, D, U, A, D, U] => False
    			 icelandic [E, I, O, OE, AA, TT, M, D, U, OE, D, U] => True
-}
icelandic :: Automaton Int IcelandicCV
icelandic = (1, [1, 2, 3], [
     (1, I, 1), (2, I, 2), (3, I, 3),
     (1, E, 1), (2, E, 2), (3, E, 3),
     (1, O, 1), (2, O, 2), (3, O, 3),
     (1, OE, 1),(2, OE, 2),(3, OE, 3),
     (1, AU, 1),(2, AU, 2),(3, AU, 3),
     (1, AA, 1),(2, AA, 2),(3, AA, 3),
     (1, P, 1), (2, U, 2), (3, A, 3),
     (1, K, 1), (2, P, 2), (3, P, 3),
     (1, L, 1), (2, K, 2), (3, K, 3),
     (1, M, 1), (2, L, 2), (3, L, 3),
     (1, D, 1), (2, M, 2), (3, M, 3),
     (1, TH, 1),(2, D, 2), (3, D, 3),
     (1, TT, 1),(2, TH, 2),(3, TH, 3),
                (2, TT, 2),(3, TT, 3),
     (1, U, 2), (2, A, 3), (1, A, 3)])


-------------------------------------------------------------------------------
-- Question 2:
-------------------------------------------------------------------------------
--CFG has repeated terminal rules, but it works.

--Test FSA
fsa1 :: Automaton Int IcelandicCV
fsa1 = (0, [1, 2], [
     (0, A, 1), (1, P, 2), (0, P, 2), (1, M, 1) ])

--fsaToCFG fsa1

{- Corresponding CFG for fsa1
0 -> 1 1
0 -> 2 2
1 -> 1 1
1 -> 2 2
1 -> A
1 -> M
2 -> P
1 -> Nothing
2 -> Nothing

Derivation for string: "AMP"
0 -> 1 1
     A 1 1
       M 1 1
         P Nothing
-}

--puts all the helper func together
fsaToCFG :: Automaton state alphabet
         -> CFG (Symbol state alphabet) (Maybe alphabet)
fsaToCFG (start, finals, trans)= (NT start, (final_state_Ts finals)++(convert_trans trans)++(get_terminals trans))

--converts final states into terminal rules of form N -> Nothing
final_state_Ts :: [nt] -> [RewriteRule (Symbol nt t) (Maybe a)]
final_state_Ts [] = []
final_state_Ts (hd:tl) = (TerminalRule (NT hd) (Nothing)):(final_state_Ts tl)

--converts transitions into nonterminal rules
convert_trans :: [(nt, t, nt)] -> [RewriteRule (Symbol nt t) (Maybe a)]
convert_trans [] = []
convert_trans (hd:tl) = case hd of (lhs, _, rhs) -> (NonterminalRule (NT lhs) [(NT rhs), (NT rhs)]):convert_trans tl

--converts terminal symbols into rules
--get_terminals :: [(nt, t, nt)] -> [t] -> [RewriteRule (Symbol nt t) (Maybe a)]
get_terminals [] = []
get_terminals (hd:tl)= case hd of (lhs, terminal, rhs) -> (TerminalRule (NT rhs) (Just terminal)):(get_terminals tl)


-------------------------------------------------------------------------------
-- Question 3:
-------------------------------------------------------------------------------
--TEST PCFG
pcfg1 :: ProbCFG String String
pcfg1 = ( [("S", 0.5)]
       , [ (NonterminalRule "S" ["NP", "VP"], 0.5)
         , (NonterminalRule "NP" ["D", "N"], 0.03)
         , (NonterminalRule "VP" ["V", "NP"], 0.04)
         , (TerminalRule "NP" "John", 0.01)
         , (TerminalRule "NP" "Mary", 0.02)
         , (TerminalRule "D" "the", 0.065)
         , (TerminalRule "D" "a", 0.023)
         , (TerminalRule "N" "cat", 0.015)
         , (TerminalRule "N" "dog", 0.079)
         , (TerminalRule "V" "saw", 0.14)
         , (TerminalRule "V" "likes", 0.098)
         ]
       )
{-
inside value: probability that derivation starting from NT will end up in at the string
	base case: empty string => 0
	base case: string is length 1 => return prob of trw
	inside(x1...x2, c) = ntrwP(c, c1, c2) * inside(x1...xk, c1) * inside(xk+1...xn, c2)

test cases:
insideP pcfg1 [] "S" 			//Empty string 			==> 0
insideP pcfg1 ["Mary"] "S" 		//NT doesn't match 		==> 0
insideP pcfg1 ["Mary"] "NP" 							==> 0.02
insideP pcfg1 ["Mary", "saw", "John"] "S"				==>	//
-}

--calls my_insideP function to get the final value and wraps it in Maybe value
insideP :: (Eq nt, Eq t) => ProbCFG nt t -> [t] -> nt -> Maybe Double
insideP _ [] _ = Just 0
insideP gram string nonT = if (not (isCNF gram)) then Nothing else case gram of (start, rules) -> Just (my_insideP rules string nonT)

--calculates the inside Value (need to fix)
my_insideP :: (Eq nt, Eq t) => [(RewriteRule nt t, Double)] -> [t] -> nt -> Double
my_insideP [] _ _ = 0
my_insideP rules (s:[]) nont = trwP rules s nont
-- my_insideP rules string nont = (ntrwP rules string nont) * (my_inside (take k str) c1) * (my_inside cfg (drop k str) c2) //Multiply for final value

--gets the probablility of the Terminal Rewrite Rule
trwP :: (Eq nt, Eq t) => [(RewriteRule nt t, Double)] -> t -> nt -> Double
trwP [] s nont = 0
trwP (r:rs) s nont = case r of (TerminalRule x y, value) -> if (x == nont && y == s) then value else trwP rs s nont
                               _ -> trwP rs s nont

--gets the probablility of the Nonterminal Rewrite Rule -- need to fix
ntrwP :: (Eq nt, Eq t) => [(RewriteRule nt t, Double)] -> nt -> [nt] -> Double
ntrwP [] _ _ = 0
ntrwP (r:rs) c c12 = case r of (NonterminalRule x y, value) -> if (c == x && y == c12) then value else 0

--Helper function for isCNF that checks if each rule in the PCFG is in CNF
is_good:: [(RewriteRule nt t, Double)] -> Bool
is_good [] = True
is_good (r:rs) = case r of (NonterminalRule x y, value) -> if (length y /= 2) then False else is_good rs
                           (TerminalRule x y, value) -> is_good rs

-- Hint: It might be helpful to write this helper function...
isCNF :: ProbCFG nt t -> Bool
isCNF (starting, rules) = if is_good rules then True else False


-------------------------------------------------------------------------------
-- Question 4:
-------------------------------------------------------------------------------
-- NOTE: [ParseStep nt t]   = parse
--       [[ParseStep nt t]] = list of parses

{-
Unfinished, code did not compile/work :(

Overall idea:

//Parser is wrapper function that tries to apply
//all the transitions to the string to get list of parses
parser = [(NoTransition, NoRule, ([], []))] ++ applyTrans (string)]

//applyTrans calls all the transitions on the strings and tries them all
//returns the list of parses
applyTrans = shift (str) ++ match (str) ++ connect (str) ++ predict (str)

//if shift is applicable, then apply shift and concat to next transition
shift str = if (possible shift) then [(Shift, ...)]++applyTrans else applyTrans

//if match is applicable, then apply match and concat to next transition
shift str = if (possible match) then [(Match, ...)]++applyTrans else applyTrans

//if connect is applicable, then apply connect and concat to next transition
shift str = if (possible connect) then [(Connect, ...)]++applyTrans else applyTrans

//if predict is applicable, then apply predict and concat to next transition
shift str = if (possible Predict) then [(Predict, ...)]++applyTrans else applyTrans

//Additional helper functions to check if transitions are applicable
-}
--applies all the transitions to the string so that all possible parses are outputted
parser :: (Eq nt, Eq t) => CFG nt t -> [t] -> [[ParseStep nt t]]
parser (start, rules) string = [[(NoTransition, NoRule, ([], []))]]

-------------------------------------------------------------------------------
-- Question 5:
-------------------------------------------------------------------------------

--similar to parser, but as it applys a transition, updates probability
parserP :: (Eq nt, Eq t) => ProbCFG nt t -> [t] -> [[ProbParseStep nt t]]
parserP = undefined

--Product of the probabilities of the rules that have been used
parseToP :: [ProbParseStep nt t] -> Double
parseToP [] = 0
parseToP (((_, (_, value),_)):[]) = value
parseToP (hd:tl) = (parseToP [hd])*(parseToP tl)
