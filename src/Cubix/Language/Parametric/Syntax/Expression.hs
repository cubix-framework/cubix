{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TemplateHaskell       #-}

module Cubix.Language.Parametric.Syntax.Expression
  (
  -- * Expressions
    ExpressionL
  , SeqOpL
  , SeqOp(..)

  -- ** Constructors and patterns
  , pattern SeqOp'
  ,        iSeqOp
  ,        jSeqOp

  -- * Operators
  , OperatorL
  , Operator(..)
  , pattern Unary'
  ,        iUnary
  ,        jUnary
  , pattern Binary'
  ,        iBinary
  ,        jBinary
  , pattern Ternary'
  ,        iTernary
  ,        jTernary

  -- ** Unary operators
  , UnaryOpL

  -- *** Constructors and patterns
  , UnaryMinusOp(..)
  , pattern UnaryMinus'
  ,        iUnaryMinus
  ,        jUnaryMinus

  , UnaryPlusOp(..)
  , pattern UnaryPlus'
  ,        iUnaryPlus
  ,        jUnaryPlus

  , ComplementOp(..)
  , pattern Complement'
  ,        iComplement
  ,        jComplement

  , LogicalNegationOp(..)
  , pattern Not'
  ,        iNot
  ,        jNot

  -- ** Binary operators
  , BinaryOpL
  
  -- *** Constructors and patterns
  , ArithBinOp(..)
  , pattern Add'
  ,        iAdd
  ,        jAdd
  , pattern Sub'
  ,        iSub
  ,        jSub
  , pattern Mul'
  ,        iMul
  ,        jMul

  , DivOp(..)
  , pattern Div'
  ,        iDiv
  ,        jDiv

  , ModOp(..)
  , pattern Mod'
  ,        iMod
  ,        jMod

  , IDivOp(..)
  , pattern IDiv'
  ,        iIDiv
  ,        jIDiv

  , ExpOp(..)
  , pattern Pow'
  ,        iPow
  ,        jPow

  , BitwiseBinOp(..)
  , pattern BitAnd'
  ,        iBitAnd
  ,        jBitAnd
  , pattern BitOr'
  ,        iBitOr
  ,        jBitOr
  , pattern BitXor'
  ,        iBitXor
  ,        jBitXor

  , LogicalBinOp(..)
  , pattern LogicAnd'
  ,        iLogicAnd
  ,        jLogicAnd
  , pattern LogicOr'
  ,        iLogicOr
  ,        jLogicOr

  , ArithShrOp(..)
  , pattern ArithShr'
  ,        iArithShr
  ,        jArithShr

  , LogicalShrOp(..)
  , pattern LogicShr'
  ,        iLogicShr
  ,        jLogicShr

  , ShlOp(..)
  , pattern Shl'
  ,        iShl
  ,        jShl

  , RelationalBinOp(..)
  , pattern Eq'
  ,        iEq
  ,        jEq
  , pattern Neq'
  ,        iNeq
  ,        jNeq
  , pattern Lt'
  ,        iLt
  ,        jLt
  , pattern Lte'
  ,        iLte
  ,        jLte
  , pattern Gt'
  ,        iGt
  ,        jGt
  , pattern Gte'
  ,        iGte
  ,        jGte

  -- * Ternary operators
  , TernaryOpL

  -- ** Constructors and patterns
  , CondTernaryOp(..)
  , pattern ITE'
  ,        iITE
  ,        jITE

  -- * Assignment to binary relation
  , AssociatedBinaryOp(..)
  , AssignOpArbitrary(..)
  , AssignOpToBinaryOp(..)
  ) where

import Data.Comp.Multi ( Node, (:-<:), All, Signature, Term, Sum, caseCxt)
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.Syntax.VarDecl

data ExpressionL

-- |
-- This is often @_,_@ or @_;_@ binary operator.
--
-- For expression @e1 , e2@ the meaning is: evaluate @e1@, then
-- evaluate @e2@, the value for the expression is the @e2@.
data SeqOpL
data SeqOp e l where
  SeqOp :: e ExpressionL -> e ExpressionL -> SeqOp e SeqOpL

deriveAll [''SeqOp]

-- | Unary operators
--
-- This represent unary prefix operator syntax form: @◇ e@, where @◇@
-- is the exact specific operator. It will evaluate arbitrary
-- expression @e@, possibly with side effects, before interpreting the
-- operator @◇@.
-- See: 'UnaryMinusOp', 'UnaryPlusOp', 'UnaryComplementOp',
-- 'LogicalNegationOp', for concreete instances of @◇@.
data UnaryOpL

-- | Unary minus prefix operator
--
-- Represents syntax: @- e@. It is left unspecified what it actually means.
-- See: 'UnaryOpL'
data UnaryMinusOp :: Node where
  UnaryMinus :: UnaryMinusOp e UnaryOpL

-- | Unary plus prefix operator
--
-- Represents syntax: @+ e@. It is left unspecified what it actually means.
-- See: 'UnaryOpL'
data UnaryPlusOp :: Node where
  UnaryPlus :: UnaryPlusOp e UnaryOpL

-- | Binary complement operator
--
-- Represents syntax: @~ e@. It is left unspecified what it actually means.
-- See: 'UnaryOpL'
data ComplementOp :: Node where
  Complement :: ComplementOp e UnaryOpL

-- | Logical negation operator
--
-- Represents syntax: @! e@. It is left unspecified what it actually means.
-- See: 'UnaryOpL'
data LogicalNegationOp :: Node where
  Not :: LogicalNegationOp e UnaryOpL

deriveAll
  [ ''UnaryMinusOp, ''UnaryPlusOp, ''ComplementOp, ''LogicalNegationOp ]

-- | Binary operators
--
-- This represents binary infix operator syntax form: @a ◇ b@, where
-- @◇@ is the exact specific operator. It will evaluate expression
-- @a@, then expression @b@ in order, both possibly with side effects,
-- before interpreting the operator @◇@.
-- See: 'ArithBinOp', 'DivOp', 'ModOp', 'IDivOp', 'ExpOp',
-- 'BitwiseBinOp', 'LogicalBinOp', 'ArithShrOp', 'LogicalShrOp',
-- 'ShlOp', 'RelationalBinOp'.
data BinaryOpL

-- | Common binary arithmetic operators
-- See: 'BinaryOpL'
data ArithBinOp :: Node where
  -- | Represents syntax: @a + b@. It is left unspecified what it
  -- actually means.
  Add :: ArithBinOp e BinaryOpL
  -- | Represents syntax: @a - b@. It is left unspecified what it
  -- actually means.
  Sub :: ArithBinOp e BinaryOpL
  -- | Represents syntax: @a * b@. It is left unspecified what it
  -- actually means.
  Mul :: ArithBinOp e BinaryOpL

-- | Division operator
--
-- Represents syntax: @a / b@. It is left unspecified what it actually
-- means.
-- See: 'BinaryOpL'
data DivOp :: Node where
  Div :: DivOp e BinaryOpL

-- | Modulo operator
--
-- Represents syntax: @a % b@. It is left unspecified what it actually
-- means.
-- See: 'BinaryOpL'
data ModOp :: Node where
  Mod :: ModOp e BinaryOpL

-- | Integer division operator
--
-- Represents syntax: @a // b@. It is left unspecified what it
-- actually means.
-- See: 'BinaryOpL'
data IDivOp :: Node where
  IDiv :: IDivOp e BinaryOpL

-- | Exponentiation operator
--
-- Represents syntax: @a ^ b@. It is left unspecified what it actually
-- means.
-- See: 'BinaryOpL'
data ExpOp :: Node where
  Pow :: ExpOp e BinaryOpL

deriveAll [''ArithBinOp, ''DivOp, ''ModOp, ''IDivOp, ''ExpOp ]

-- | Common bit manipulation operators
-- See: 'BinaryOpL'
data BitwiseBinOp :: Node where
  -- | Represents syntax: @a & b@. It is left unspecified what it
  -- actually means.
  BitAnd :: BitwiseBinOp e BinaryOpL
  -- | Represents syntax: @a | b@. It is left unspecified what it
  -- actually means.
  BitOr  :: BitwiseBinOp e BinaryOpL
  -- | Represents syntax: @a ^ b@. It is left unspecified what it
  -- actually means.
  BitXor :: BitwiseBinOp e BinaryOpL

-- | Common logical operators
-- See: 'BinaryOpL'
data LogicalBinOp :: Node where
  -- | Represents syntax: @a && b@. It is left unspecified what it
  -- actually means.
  LogicAnd :: LogicalBinOp e BinaryOpL
  -- | Represents syntax: @a || b@. It is left unspecified what it
  -- actually means.
  LogicOr  :: LogicalBinOp e BinaryOpL

-- | Arithmetic shift right operator
--
-- | Represents syntax: @a >>> b@. It is left unspecified what it
-- actually means.
-- See: 'BinOpL'
data ArithShrOp :: Node where
  ArithShr :: ArithShrOp e BinaryOpL

-- | Logical shift right operator
--
-- | Represents syntax: @a >> b@. It is left unspecified what it
-- actually means.
-- See: 'BinOpL'
data LogicalShrOp :: Node where
  LogicShr :: LogicalShrOp e BinaryOpL

-- | Shift left operator
--
-- | Represents syntax: @a << b@. It is left unspecified what it
-- actually means.
-- See: 'BinOpL'
data ShlOp :: Node where
  Shl :: ShlOp e BinaryOpL

deriveAll
  [''BitwiseBinOp, ''LogicalBinOp, ''ArithShrOp, ''LogicalShrOp, ''ShlOp]

-- | Relational (comparison) operators
-- See: 'BinOpL'
data RelationalBinOp :: Node where
  -- | Represents syntax: @a == b@. It is left unspecified what it
  -- actually means.
  Eq  :: RelationalBinOp e BinaryOpL
  -- | Represents syntax: @a != b@. It is left unspecified what it
  -- actually means.
  Neq :: RelationalBinOp e BinaryOpL
  -- | Represents syntax: @a < b@. It is left unspecified what it
  -- actually means.
  Lt  :: RelationalBinOp e BinaryOpL
  -- | Represents syntax: @a <= b@. It is left unspecified what it
  -- actually means.
  Lte :: RelationalBinOp e BinaryOpL
  -- | Represents syntax: @a > b@. It is left unspecified what it
  -- actually means.
  Gt  :: RelationalBinOp e BinaryOpL
  -- | Represents syntax: @a >= b@. It is left unspecified what it
  -- actually means.
  Gte :: RelationalBinOp e BinaryOpL

deriveAll  [''RelationalBinOp]

-- | Ternary operators
--
-- This represents operators with 3 arguments.
data TernaryOpL

-- | This represents special syntax form @cond ? a : b@, where cond is
-- an expression returning value that mighty be @thruthy@ or
-- @falsy@. @cond@ is evaluated first, depending on the result, either
-- one of @a@ or @b@ gets evaluated. The return value of this ternary
-- operator is the result of the last evaluated expression.
data CondTernaryOp :: Node where
  ITE :: CondTernaryOp e TernaryOpL

deriveAll [''CondTernaryOp]

-- | Operator expressions
--
-- Expressions that are formed by the use of the special syntax (operators).
-- See 'UnaryOpL', 'BinaryOpL' and 'TernaryOpL'
data OperatorL
data Operator e l where
  Unary   :: e UnaryOpL
          -> e ExpressionL
          -> Operator e ExpressionL
  Binary  :: e BinaryOpL
          -> e ExpressionL -> e ExpressionL
          -> Operator e ExpressionL
  Ternary :: e TernaryOpL
          -> e ExpressionL -> e ExpressionL -> e ExpressionL
          -> Operator e ExpressionL

deriveAll [''Operator]


------------------------------------------------------------------------------------
--------------  Relating assignment operators with binary operators  ---------------
------------------------------------------------------------------------------------

data AssociatedBinaryOp fs = NoOp | SomeOp (Term fs BinaryOpL)

data AssignOpArbitrary e l where
  AssignOpArbitrary :: e BinaryOpL -> AssignOpArbitrary e AssignOpL

class AssignOpToBinaryOp (fs :: Signature) (f :: Node) where
  assignOpToBinaryOp :: f (Term fs) AssignOpL -> AssociatedBinaryOp fs

instance {-# OVERLAPPABLE #-} AssignOpToBinaryOp fs f where
  assignOpToBinaryOp = const NoOp

instance All (AssignOpToBinaryOp fs) fs => AssignOpToBinaryOp fs (Sum fs) where
  assignOpToBinaryOp = caseCxt @(AssignOpToBinaryOp fs) assignOpToBinaryOp

instance ArithBinOp :-<: fs => AssignOpToBinaryOp fs AssignOpAdd where
  assignOpToBinaryOp AssignOpAdd = SomeOp Add'

instance ArithBinOp :-<: fs => AssignOpToBinaryOp fs AssignOpSub where
  assignOpToBinaryOp AssignOpSub = SomeOp Sub'

instance ArithBinOp :-<: fs => AssignOpToBinaryOp fs AssignOpMul where
  assignOpToBinaryOp AssignOpMul = SomeOp Mul'

instance DivOp :-<: fs => AssignOpToBinaryOp fs AssignOpDiv where
  assignOpToBinaryOp AssignOpDiv = SomeOp Div'

instance ModOp :-<: fs => AssignOpToBinaryOp fs AssignOpMod where
  assignOpToBinaryOp AssignOpMod = SomeOp Mod'

instance ArithShrOp :-<: fs => AssignOpToBinaryOp fs AssignOpArithShr where
  assignOpToBinaryOp AssignOpArithShr = SomeOp ArithShr'

instance LogicalShrOp :-<: fs => AssignOpToBinaryOp fs AssignOpLogicShr where
  assignOpToBinaryOp AssignOpLogicShr = SomeOp LogicShr'

instance ShlOp :-<: fs => AssignOpToBinaryOp fs AssignOpShl where
  assignOpToBinaryOp AssignOpShl = SomeOp Shl'

instance BitwiseBinOp :-<: fs => AssignOpToBinaryOp fs AssignOpBitAnd where
  assignOpToBinaryOp AssignOpBitAnd = SomeOp BitAnd'

instance BitwiseBinOp :-<: fs => AssignOpToBinaryOp fs AssignOpBitOr where
  assignOpToBinaryOp AssignOpBitOr = SomeOp BitOr'

instance BitwiseBinOp :-<: fs => AssignOpToBinaryOp fs AssignOpBitXor where
  assignOpToBinaryOp AssignOpBitXor = SomeOp BitXor'

