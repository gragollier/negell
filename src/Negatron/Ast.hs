module Negatron.Ast where
import Data.Text (Text)
import qualified Data.Map.Strict as Map

type PFunctionMap = Map.Map Text Function

data BinOp = Sub
           | Mult
           | Div
           | Eq
           | Neq
           | Less
           | Leq
           | Greater
           | Geq
           | And
           | Or
           deriving (Show, Eq)

data UnOp = Neg
          | Not
          deriving (Show, Eq)

data Expr = IntLit Int
          | StrLit Text
          | BoolLit Bool
          | Null
          | Id Text
          | Bin BinOp Expr Expr
          | Un UnOp Expr
          | Call Text [Expr]
          | Assign Text Expr
          | Noexpr
          deriving (Show, Eq)

data Statement = Expr Expr
               | Block [Statement]
               | Return Expr
               | StmtDecl Decl
               | If Expr Statement Statement
               | While Expr Statement
               | Output Expr
               | Input Text
               deriving (Show, Eq)

data Type = IntType
          | BoolType
          | VoidType
          deriving (Show, Eq)

data Decl = Decl { declType :: Type, declName :: Text }
            deriving (Show, Eq)

data Function = Function
    {   typ :: Type,
        name :: Text,
        formals :: [Decl],
        locals :: [Decl],
        body :: [Statement]
    } deriving (Show, Eq)

data Program = Program PFunctionMap [Decl] deriving (Show, Eq)
