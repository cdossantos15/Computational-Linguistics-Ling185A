module CFG where

-- Nonprobabilistic CFGs.
type CFG nt t = ( nt                  -- Starting nonterminal.
                , [RewriteRule nt t]  -- List of rules.
                )

-- Probabilistic CFGs.
-- NOTE: Every potential starting nonterminal and every rule is paired with
--       a probability.
type ProbCFG nt t = ( [(nt, Double)]                -- Starting nonterminals.
                    , [(RewriteRule nt t, Double)]  -- List of rules.
                    )

-- NOTE: NonterminalRule represents a nonterminal rewriting to a series of
--       nonterminals. Thus, this type does not enforce Chomsky Normal Form.
data RewriteRule nt t = NonterminalRule nt [nt]
                      | TerminalRule nt t
                      | NoRule
                      deriving (Show, Eq)

-- This allows terminal and nonterminal nodes/symbols to be different types.
-- Examples: NT 3 :: Symbol Int Char
--           T 'a' :: Symbol Int Char
data Symbol nt t = NT nt | T t deriving (Show, Eq)
