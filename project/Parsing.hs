module Parsing where

import CFG

-- Transitions for left-corner parser.
data Transition = Shift | Match | Predict | Connect | NoTransition
                  deriving (Eq, Show)

-- Representation of barred and not-barred symbols on the stack.
data Stack nt = Bar nt | NoBar nt deriving (Eq, Show)

-- Parse configuration.
type Config nt t = ([Stack nt], [t])

-- Nonprobabilistic parse step.
type ParseStep nt t = ( Transition        -- Transition executed.
                      , RewriteRule nt t  -- Rule invoked.
                      , Config nt t       -- Resulting configuration.
                      )

-- Nonprobabilistic parse step.
type ProbParseStep nt t = ( Transition                  -- Transition executed.
                          , (RewriteRule nt t, Double)  -- Rule invoked.
                          , Config nt t                 -- Resulting config.
                          )


--parseToP [(NoTransition , (NoRule, 1.0), ([],[])), (NoTransition , (NoRule, 2.0), ([],[])), (NoTransition , (NoRule, 3.0), ([],[])),(NoTransition , (NoRule, 4.0), ([],[]))]