module Expr where

import Text.Parsec
import Text.ParserCombinators.Parsec

data GeneralDefModifier
  = Alias
  | Parameterized
  | Enum
  | Object
  | Extend
  | Trait
  | Impl
  | Replace
  | Mod
  deriving (Eq, Show)

type Generics = [Expr]

type RefinementExpr = Expr

type TypeExpr = Expr

type RawIdentExpr = String

type IdentifierExpr = Expr

type UnaryExpr = Expr

type GeneralDefRhs = Expr

type ImplRefinement = [Expr]

type MultipleParamExpr = [Expr]

data GenericParamType = Dependent | TypeParam deriving (Eq, Show)

type EnumItem = Expr

type TraitExpr = Expr

type ImplItem = Expr

type Args = [Expr]

type Module = String

data LoopType = For | While deriving (Eq, Show)

data Expr
  = LiteralExpr Primitive
  | IdentExpr [String]
  | BinaryOp OverloableBinaryOperator Expr Expr
  | UnaryOp OverloableUnaryOperator Expr UnaryType
  | ParenExpr Expr
  | GeneralDef RawIdentExpr Generics GeneralDefRhs
  | Param IdentifierExpr TypeExpr [RefinementExpr]
  | Refinement UnaryExpr
  | GenericParam (Maybe RawIdentExpr) IdentifierExpr ImplRefinement GenericParamType
  | Call Expr Args
  | EvalExpr Expr
  | ParameterizedExpr [Expr] ReturnType Expr
  | Annotation Args
  | MacroBodyExpr Expr
  | VariableDef TypeModifier Expr Expr
  | VariableRedef Expr Expr
  | ExportExpr Module
  | Require Namespace [ImportItem]
  | If Expr Expr Expr
  | Loop LoopType Expr Expr
  | ScopeExpr [Expr]
  | ArrowExpr Expr
  deriving (Eq, Show)

data UnaryType = Prefix | Postfix deriving (Eq, Show)

type ObjType = Expr

type ReturnType = Expr

newtype ReiNumeric = Numeric {val :: String} deriving (Eq, Show)

class ToString a where
  toString :: a -> String

type Primitive = String

type Namespace = String

type ImportItem = String

data TypeModifier = Let | Mut | Const deriving (Eq, Show)

data ReservedOperator = DollarSign | At | Hash | Eq | Dot | Comma | QuestionMark | Exclamation | Elvis | UnconditionalPropagate Bool | Colon | Semicolon | DoubleColon deriving (Eq, Show)

data OverloableUnaryOperator = PlusPlus | MinusMinus deriving (Eq, Show)

data OverloableBinaryOperator
  = Equiv
  | Lt
  | Gt
  | Gte
  | Lte
  | And
  | Or
  | Mult
  | Div
  | Add
  | Sub
  | Modulo
  | PlusEq
  | MinusEq
  | MultEq
  | DivEq
  | AndEq
  | OrEq
  | XorEq
  | DoubleMult
  | DoubleDiv
  | LeftShift
  | RightShift
  | BitwiseAnd
  | BitwiseOr
  | BitwiseNot
  | BitwiseXor
  deriving (Eq, Show)

--------------------
--  PARSING
--------------------

parameterisedExpr :: Parser String
parameterisedExpr = many1 parenParamList

parenParamList :: Parser String
parenParamList = do
  char '('
  param <- many1 anyChar
  char ')'
  return param
