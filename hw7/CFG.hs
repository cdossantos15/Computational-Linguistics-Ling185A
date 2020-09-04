module CFG where

import Data.List (nub)

-------------------------------------------------------------------------------

-- Make sure that you understand how this represents a CFG.
type CFG nt t = ( nt                  -- Starting nonterminal.
                , [RewriteRule nt t]  -- List of rules.
                )

-- Rules that enforce Chomsky Normal Form.
data RewriteRule nt t = NonterminalRule nt (nt, nt)
                      | TerminalRule nt t
                      deriving (Show, Eq)

-- This allows terminal and nonterminal nodes to be different types.
-- Examples: NT 3 :: Symbol Int Char
--           T 'a' :: Symbol Int Char
data Symbol nt t = NT nt | T t deriving (Show, Eq)

-- Definition of a type for tree structures.
-- Example: NonLeaf "S" (NonLeaf "NP" (Leaf "D" "the") (Leaf "N" "cat"))
--              (NonLeaf "VP" (Leaf "V" "saw") (Leaf "NP" "Mary"))
--          Above, the type of the expression is Tree String String.
data Tree nt t = NonLeaf nt (Tree nt t) (Tree nt t)
               | Leaf nt t
               deriving (Show, Eq)

-------------------------------------------------------------------------------

cfg1 :: CFG String String
cfg1 = ( "S"
       , [ NonterminalRule "S" ("NP", "VP")
         , NonterminalRule "NP" ("D", "N")
         , NonterminalRule "VP" ("V", "NP")
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

-- This function splits at the leftmost occurrence of the given nonterminal,
-- which is not necessarily the same as splitting at the leftmost nonterminal.
splitAtNT :: (Eq nt, Eq t)
          => nt
          -> [Symbol nt t]
          -> Maybe ([Symbol nt t], [Symbol nt t])
splitAtNT _ [] = Nothing
splitAtNT key (x:xs)
    | x == NT key = Just ([], xs)
    | otherwise   = case splitAtNT key xs of
        Nothing       -> Nothing
        Just (ys, zs) -> Just (x:ys, zs)

-- Example usage:
-- rewrite (A -> beta) (alpha A gamma) ==> alpha beta gamma
-- rewrite (NP -> D N) [S, Mary, NP, the] ==> Just [S, Mary, D, N, the]
-- rewrite (NP -> D N) [S, Mary, VP, the] ==> Nothing
rewrite :: (Eq nt, Eq t)
        => RewriteRule nt t
        -> [Symbol nt t]
        -> Maybe [Symbol nt t]
rewrite r xs =
    case splitAtNT a xs of
        Nothing             -> Nothing
        Just (alpha, gamma) -> Just (concat [alpha, beta, gamma])
  where
    (a, beta) = case r of NonterminalRule x (y, z) -> (x, [NT y, NT z])
                          TerminalRule x y -> (x, [T y])

foo :: Maybe [Symbol String String]
foo = rewrite (NonterminalRule "NP" ("D", "N"))
              [NT "S", T "Mary", NT "NP", T "the"]

-------------------------------------------------------------------------------

-- Corresponds to (8a) on the handout.
init' :: Ord nt => CFG nt t -> nt -> Bool
init' (start, rules) c = c == start

-- Corresponds to (8b) on the handout.
trw :: (Ord nt, Ord t) => CFG nt t -> nt -> t -> Bool
trw (start, rules) c x = elem (TerminalRule c x) rules

-- Corresponds to (8c) on the handout.
ntrw :: (Ord nt, Ord t) => CFG nt t -> nt -> nt -> nt -> Bool
ntrw (start, rules) c c1 c2 = elem (NonterminalRule c (c1, c2)) rules

-- This function is necessary for the ranges on the big disjunctions.
allNTs :: (Ord nt) => CFG nt t -> [nt]
allNTs (start, rules) = nub [case r of NonterminalRule x (y,z) -> x
                                       TerminalRule x y -> x
                             | r <- rules]

-- Corresponds to (9) on the handout.
inside :: (Ord nt, Ord t) => CFG nt t -> [t] -> nt -> Bool
inside _ [] _       = False
inside cfg (x:[]) c = trw cfg c x
inside cfg str c    =
    or [    ntrw cfg c c1 c2
         && inside cfg (take k str) c1
         && inside cfg (drop k str) c2
        | c1 <- allNTs cfg, c2 <- allNTs cfg,
          k <- [1 .. length str - 1]
       ]

-------------------------------------------------------------------------------
-- Several helper functions:

-- Returns root node of a tree.
root :: Tree nt t -> nt
root (Leaf x y) = x
root (NonLeaf x y z) = x

-- Returns the left-hand side of a rule.
lhs :: RewriteRule nt t -> nt
lhs (NonterminalRule x (y, z)) = x
lhs (TerminalRule x y) = x

-- Returns the right-hand side of a rule.
rhs :: RewriteRule nt t -> [Symbol nt t]
rhs (NonterminalRule x (y, z)) = [NT y, NT z]
rhs (TerminalRule x y) = [T y]
