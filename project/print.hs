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
