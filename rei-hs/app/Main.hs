module Main where

import Text.Parsec

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"

stringParser:: Parsec String st String
stringParser = many anyChar

data GeneralDefModifier = Alias
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

data Expr = LiteralExpr Primitive
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
          | Enum [EnumItem]
          | Complex [Expr]
          | Trait IdentifierExpr [TraitExpr]
          | Impl IdentifierExpr [ImplItem]
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
