{- |
   Propositional formulas
-}
module Logic.Propositions where

-- | Mark for literals
data Literal a
  = P {getAtom :: a}
  | N {getAtom :: a} deriving Eq

complement :: Literal a -> Literal a
complement (P x) = N x
complement (N x) = P x

areComplement :: Eq a => Literal a -> Literal a -> Bool
areComplement (P x) (N y) = x == y
areComplement (N x) (P y) = x == y
areComplement _ _ = False

instance Show a => Show (Literal a) where
  show (P a) = show a
  show (N a) = "~" ++ show a

-- | Infix formula.
-- More readable, more ambiguous.
data IFormula a
  = Z
  | O
  | Var a
  | Neg (IFormula a)
  | (IFormula a) :&: (IFormula a)
  | (IFormula a) :|: (IFormula a)
  | (IFormula a) :->: (IFormula a)
  | (IFormula a) :<->: (IFormula a)
  deriving (Eq, Show)

infixr 9 :&:

infixr 8 :|:

infixr 7 :->:

infixr 6 :<->:

-- | Transforms an infix formula to the corresponding prefix formula
--
-- Examples:
-- >>> fromInfix Z
-- F
--
-- >>> fromInfix $ Var "a" :<->: Neg (Var "b")
-- ("a" <-> ¬"b")
--
-- >>> fromInfix $ Var "p" :&: Var "q"
-- ("p" & "q")
fromInfix :: IFormula a -> Formula a
fromInfix Z = F
fromInfix O = T
fromInfix (Var v) = Atom v
fromInfix (Neg p) = Not $ fromInfix p
fromInfix (p :&: q) = Bin Conj (fromInfix p) (fromInfix q)
fromInfix (p :|: q) = Bin Dis (fromInfix p) (fromInfix q)
fromInfix (p :->: q) = Bin Impl (fromInfix p) (fromInfix q)
fromInfix (p :<->: q) = Bin Equiv (fromInfix p) (fromInfix q)

-- | Allowed binary operators for formulas
data Op
  = Dis -- ^ Disjunction (Or)
  | Conj -- ^ Conjunction (And)
  | Impl -- ^ Implication
  | Equiv -- ^ Logical equivalence
  deriving (Eq, Enum)

-- | Propositional formulas.
-- Using polish notation
data Formula a
  = T
  | F
  | Atom a
  | Not (Formula a)
  | Bin Op (Formula a) (Formula a)
  deriving (Eq)

instance Show Op where
  show Dis = "|"
  show Conj = "&"
  show Impl = "->"
  show Equiv = "<->"

instance Show a => Show (Formula a) where
  show T = "T"
  show F = "F"
  show (Atom a) = show a
  show (Not p) = "¬" ++ show p
  show (Bin op l r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
