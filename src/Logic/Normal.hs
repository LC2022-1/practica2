{- |
  Normal forms for propositional formulas
-}
module Logic.Normal where

import Logic.Propositions

import Data.Maybe (isJust)
import Data.List (nub)

-- | Transform the formula into an equivalent formula in negative normal form.
-- A formula is in negative normal form if it has only disjuntions or
-- conjunctions and all the negations are applied to variables.
--
-- Examples:
-- >>> toNnf $ Atom "p"
-- "p"
--
-- >>> toNnf . fromInfix $ Var "p" :<->: Var "q"
-- ((¬"p" | "q") & (¬"q" | "p"))
--
-- >>> toNnf . fromInfix $ Neg (Var "p" :|: Var "q") :<->: Var "r"
-- ((("p" | "q") | "r") & (¬"r" | (¬"p" & ¬"q")))
--
toNnf :: Formula a -> Formula a
toNnf = undefined

-- | Transforms the given formula into its equivalent conjuntive normal form
-- This can be done by transforming first to a NNF and then using distruibutive
-- rules to obtain a conjuntion of disjuntions.
--
-- Examples:
-- >>> toCnf . fromInfix $ Neg (Var "p" :|: Var "q")
-- (¬"p" & ¬"q")
--
-- >>> toCnf . fromInfix $ (Var "p" :|: Var "q") :&: (Var "r" :|: Neg (Var "s"))
-- (("p" | "q") & ("r" | ¬"s"))
--
-- >>> toCnf . fromInfix $ Var "p" :|: (Var "q" :&: (Neg (Var "r")))
-- (("p" | "q") & ("p" | ¬"r"))
--
-- >>> toCnf . fromInfix $ Var "p" :<->: Neg (Var "q" :|: Var "r")
-- (((¬"p" | ¬"q") & (¬"p" | ¬"r")) & (("q" | "r") | "p"))
toCnf :: Formula a -> Formula a
toCnf = undefined

-- | Transforms the given formula into its equivalente disjunctive normal form.
-- This can be done by transforming first to a NNF and then using distruibutive
-- rules to obtain a disjuntion of conjuntions.
--
-- Examples:
-- >>> toDnf . fromInfix $ Neg (Var "p" :|: Var "q")
-- (¬"p" & ¬"q")
--
-- >>> toDnf . fromInfix $ Var "p" :&: Var "q" :|: Var "r" :&: Neg (Var "s")
-- (("p" & "q") | ("r" & ¬"s"))
--
-- >>> toDnf . fromInfix $ Var "p" :&: (Var "q" :|: (Neg (Var "r")))
-- (("p" & "q") | ("p" & ¬"r"))
--
-- >>> toDnf . fromInfix $ Var "p" :<->: Neg (Var "q")
-- (((¬"p" & "q") | (¬"p" & "p")) | ((¬"q" & "q") | (¬"q" & "p")))
toDnf :: Formula a -> Formula a
toDnf = undefined

-- | A clause is an implicit disjuntions of literals
type Clause a = [Literal a]

-- | Transform  formula into its clausulal representation. That is, CNF but
-- every disjunction is represented as a list of literals (called clause) and
-- every conjuntion is a list of clauses.
--
-- Examples:
-- >>> toClausulal . fromInfix $ Neg (Var "p" :|: Var "q")
-- [[~"p"],[~"q"]]
--
-- >>> toClausulal . fromInfix $ (Var "p" :|: Var "q") :&: (Var "r" :|: Var "s")
-- [["p","q"],["r","s"]]
--
-- >>> toClausulal . fromInfix $ Var "p" :|: Var "q" :&: (Neg (Var "r"))
-- [["p","q"],["p",~"r"]]
--
-- >>> toClausulal . fromInfix $ Var "p" :<->: Neg (Var "q" :|: Var "r")
-- [[~"p",~"q"],[~"p",~"r"],["q","r","p"]]
toClausulal :: Eq a => Formula a -> [Clause a]
toClausulal = undefined
