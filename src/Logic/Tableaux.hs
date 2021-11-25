{- |
 Naive implementation of semantic tableaux
-}
module Logic.Tableaux where

import Logic.Propositions

-- | Formula classification
data Class a
  = Alpha {left :: Formula a, right :: Formula a} -- ^ Alpha fórmula
  | Beta {left :: Formula a, right :: Formula a} -- ^ Beta formula
  | Lit {lit :: Literal a} -- ^ Logical literals
  | Na {atom :: Formula a} -- ^ For logical constants
  deriving Show

-- | Classify a formula and split its subformulas.
-- A formula can be a literal or non-literal. A non-literal formula can be
-- alpha or beta depending on its satisfacibility. The only exception are
-- logical constants wich are neither literals, alpha nor beta, so they have a
-- special label.
--
-- Examples:
-- >>> classify $ Not T
-- Na {atom = F}
--
-- >>> classify $ Not (Atom "p")
-- Lit {lit = ~"p"}
--
-- >>> classify . fromInfix $ Neg (Var "p" :->: Var "q")
-- Alpha {left = "p", right = ¬"q"}
--
-- >>> classify . fromInfix $ Neg (Var "p" :&: Var "q")
-- Beta {left = ¬"p", right = ¬"q"}
classify :: Formula a -> Class a
classify = undefined

-- | Tree structur for a semantic tableau
data Tableau a
  = Tableau
    {formulas :: [Formula a] -- ^ Set of non-literal formulas
    ,literals :: [Literal a] -- ^ Set of literals
    ,children :: [Tableau a] -- ^ Subtrees of the tableau
    }

instance Show a => Show (Tableau a) where
  show t = unlines . showLevels $ t

-- | Shows a tableau as a file-tree like structure
showLevels :: Show a => Tableau a -> [String]
showLevels (Tableau fs ls chs) =
  ("phis: " ++ show fs)
  : ("lits: " ++ show ls)
  : (tabSubTrees chs)

-- | Show subtrees using file-tree like symbols and indendation
tabSubTrees :: Show a => [Tableau a] -> [String]
tabSubTrees [] = []
tabSubTrees xs =
  (init xs >>= showPr "├── " "│   ")
  ++ (showPr "└── " "    " . last $ xs)
    where showPr f r = tabPriority f r . showLevels

-- | Add prefix to each string, with the first element getting a special
-- prefix.
tabPriority :: String -> String -> [String] -> [String]
tabPriority _ _ [] = []
tabPriority f r (x:xs) = (f ++ x) : (map (r ++) xs)

-- | Build full tableau for a given formula. That is, the expansion of the
-- tableu with the formula in its root.
--
-- Other than alpha and beta formulas, literals and logical constants have to
-- be taken into account explicitly.
--
-- For literals, just add them to the literal list. For a `true` constant, just
-- ignore it and continue expanding. For a `false` constant, terminate the
-- expansion.
--
-- Examples:
-- >>> makeTableau  . fromInfix $ Var "s"
-- phis: []
-- lits: ["s"]
-- ...
--
-- >>> makeTableau  . fromInfix $ Var "p" :&: Var "q"
-- phis: [("p" & "q")]
-- lits: []
-- └── phis: []
--     lits: ["q","p"]
-- ...
--
-- >>> makeTableau  . fromInfix $ Var "s" :&: Var "p" :|: Var "q"
-- phis: [(("s" & "p") | "q")]
-- lits: []
-- ├── phis: [("s" & "p")]
-- │   lits: []
-- │   └── phis: []
-- │       lits: ["p","s"]
-- └── phis: []
--     lits: ["q"]
-- ...
--
-- >>> makeTableau  . fromInfix $ (O :|: Var "r") :->:  Var "p"
-- phis: [((T | "r") -> "p")]
-- lits: []
-- ├── phis: [¬(T | "r")]
-- │   lits: []
-- │   └── phis: [F,¬"r"]
-- │       lits: []
-- └── phis: []
--     lits: ["p"]
-- ...
makeTableau :: Formula a -> Tableau a
makeTableau = undefined

-- | If a set of literals is satisfiable.
-- It is satisfiable if there are no complementary literals.
--
-- Examples:
-- >>> isSatisfiable []
-- True
--
-- >>> isSatisfiable [P 1, N 2, P 1, P 3]
-- True
--
-- >>> isSatisfiable [P 1, P 2, N 1]
-- False
isSatisfiable :: Eq a => [Literal a] -> Bool
isSatisfiable = undefined

-- | If a tablea has an open leaf.
-- A tableau node is open if any of its sub-tableaux is open. A leaf tableau is
-- open if its literals are satisfiable and its formulas don't contain the false
-- constant.
--
-- Examples:
-- >>> isOpen $ Tableau [] [] []
-- True
--
-- >>> isOpen $ Tableau [T, Atom "p"] [] []
-- True
--
-- >>> isOpen $ Tableau [F] [] []
-- False
--
-- >>> isOpen . makeTableau . fromInfix $ Var "p" :<->: Var "q"
-- True
--
-- >>> isOpen . makeTableau . fromInfix $ Var "p" :&: Neg (Var "p")
-- False
isOpen :: Eq a => Tableau a -> Bool
isOpen = undefined
