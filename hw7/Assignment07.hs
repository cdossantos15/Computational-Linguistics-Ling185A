module Assignment07 where

import CFG

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

-- Numero uno.


--Helper functions for terminalsOnly
check_nt :: [Symbol nt t] -> Bool
check_nt [] = False
check_nt (hd:tl) = case hd of (NT x) -> True
                              (T x) -> check_nt tl

get_symbols :: [Symbol nt t] -> [t]
get_symbols [] = []
get_symbols (hd:tl) = case hd of (T x) -> x:(get_symbols tl)


terminalsOnly :: [Symbol nt t] -> Maybe [t]
terminalsOnly [] = Nothing
terminalsOnly list = if (check_nt list == True) then Nothing
                     else (Just (get_symbols list))


-- KEEP TESTING
-- yield (NonLeaf "S" (Leaf "NP" "Mary") (Leaf "VP" "ran"))
-- yield (NonLeaf 0 (Leaf 0 'a') (NonLeaf 0 (Leaf 0 'b') (Leaf 0 'c')))
yield :: Tree nt t -> [t]
yield (Leaf _ t) = [t]
yield (NonLeaf x y z) = (yield y)++(yield z)

-- Numero dos.

treeToRuleList :: Tree nt t -> [RewriteRule nt t]
treeToRuleList (Leaf x y) = [TerminalRule x y]
treeToRuleList (NonLeaf x y z) = (NonterminalRule x ((root y),(root z)):(treeToRuleList y))++(treeToRuleList z)


--TODO:

ruleListToTree :: (Eq nt, Eq t) => [RewriteRule nt t] -> Maybe (Tree nt t)
ruleListToTree rules = undefined


treeToDerivation :: Tree nt t -> [[Symbol nt t]]
treeToDerivation tree = undefined
                                     


-- Numero tres.

areThereNTs :: (Eq nt, Eq t) => (Eq nt, Eq t) => [Symbol nt t] -> Bool
areThereNTs [] = False
areThereNTs (hd:tl) = case hd of (NT _) -> True
                                 (T _) -> areThereNTs tl

get_left_Ts :: (Eq nt, Eq t) => [Symbol nt t] -> [Symbol nt t]
get_left_Ts [] = []
get_left_Ts (hd:tl) = case hd of (NT nt) -> []
                                 (T t) -> hd:get_left_Ts tl
get_right_Ts :: (Eq nt, Eq t) => [Symbol nt t] -> [Symbol nt t]
get_right_Ts [] = []
get_right_Ts (hd:tl) = case hd of (NT nt) -> tl
                                  (T t) -> get_right_Ts tl

get_NT :: (Eq nt, Eq t) => [Symbol nt t] -> nt
get_NT (hd:tl) = case hd of (NT nt) -> nt
                            (T t) -> get_NT tl

splitAtLeftmost :: (Eq nt, Eq t)
                => [Symbol nt t] -> Maybe ([Symbol nt t], nt, [Symbol nt t])
splitAtLeftmost [] = Nothing
splitAtLeftmost list = case (areThereNTs list) of True -> Just (get_left_Ts list, get_NT list, get_right_Ts list)
                                                  False -> Nothing

get_left :: (Eq nt, Eq t) => nt -> [Symbol nt t] -> [Symbol nt t]
get_left _ [] = []
get_left target (hd:tl) = case hd of (T t) -> hd:(get_left target tl)
                                     (NT nt) -> if (nt /= target) then hd:(get_left target tl) else []

get_right :: (Eq nt, Eq t) => nt -> [Symbol nt t] -> [Symbol nt t]
get_right _ [] = []
get_right target (hd:tl) = case hd of (NT nt) -> if (nt == target) then tl else (get_right target tl)
                                      (T t) -> (get_right target tl)

check_Rule :: (Eq nt, Eq t) => nt -> RewriteRule nt t -> Bool
check_Rule target rule = if ((lhs rule) == target) then True else False

rewriteLeftmost :: (Eq nt, Eq t)
                => [RewriteRule nt t] -> [Symbol nt t] -> [[Symbol nt t]]
rewriteLeftmost _ [] = []
rewriteLeftmost [] list = []
rewriteLeftmost (r:rs) list = if (check_Rule (get_NT list) r)
                              then (((get_left (get_NT list) list)++(rhs r)++(get_right (get_NT list) list)):(rewriteLeftmost rs list) )
                              else (rewriteLeftmost rs list)







