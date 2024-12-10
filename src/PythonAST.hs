{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module PythonAST where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor (void)
import Data.Char (isSpace)
import Data.Text (Text)

-- | A representation of a Python AST that includes a wide range of language features.
data AST
    = Module [AST]                           -- A Python module containing a list of statements
    | ImportStmt [ImportItem]                -- import statements, e.g. import os, import sys
    | FromImportStmt String [ImportItem]     -- from X import Y, Z
    | ClassDef {
        className       :: String,
        classBases      :: [Expr],
        classKeywords   :: [(String, Expr)],
        classBody       :: [AST]
      }
    | FunctionDef {
        funcName       :: String,
        funcParams     :: [Param],
        funcReturns    :: Maybe Expr,   -- Type hint on return
        funcBody       :: [AST]
      }
    | AsyncFunctionDef {
        asyncFuncName       :: String,
        asyncFuncParams     :: [Param],
        asyncFuncReturns    :: Maybe Expr,
        asyncFuncBody       :: [AST]
      }
    | IfStmt {
        ifCond      :: Expr,
        ifBody      :: [AST],
        elifClauses :: [(Expr, [AST])],
        elseBody    :: [AST]
      }
    | ForStmt {
        forTarget   :: Expr,
        forIter     :: Expr,
        forBody     :: [AST],
        forElseBody :: [AST]
      }
    | AsyncForStmt {
        asyncForTarget :: Expr,
        asyncForIter   :: Expr,
        asyncForBody   :: [AST],
        asyncForElse   :: [AST]
      }
    | WhileStmt {
        whileCond :: Expr,
        whileBody :: [AST],
        whileElse :: [AST]
      }
    | TryStmt {
        tryBody      :: [AST],
        exceptClauses :: [ExceptClause],
        tryElseBody  :: [AST],
        tryFinally   :: [AST]
      }
    | WithStmt {
        withItems :: [(Expr, Maybe String)], -- (context_expr, optional_alias)
        withBody  :: [AST]
      }
    | AsyncWithStmt {
        asyncWithItems :: [(Expr, Maybe String)],
        asyncWithBody  :: [AST]
      }
    | ReturnStmt (Maybe Expr)
    | RaiseStmt (Maybe Expr) (Maybe Expr)
    | PassStmt
    | BreakStmt
    | ContinueStmt
    | GlobalStmt [String]
    | NonlocalStmt [String]
    | ExprStmt Expr
    | Assign [Expr] Expr            -- a, b = something
    | AnnAssign Expr Expr Bool      -- variable: type = value
    | AugAssign Expr AugOp Expr     -- x += 1, x -= 2, etc.
    | DeleteStmt [Expr]
    | Comment String                -- Optional: storing comments
    | DocString String              -- Optional: docstring representation
    deriving (Show, Eq)

-- | Representation of imports
data ImportItem = ImportItem {
    importName :: String,
    importAs   :: Maybe String
} deriving (Show, Eq)

-- | Function/Class parameters: includes support for positional, keyword, etc.
data Param = Param {
    paramName     :: String,
    paramTypeHint :: Maybe Expr,
    paramDefault  :: Maybe Expr
} deriving (Show, Eq)

-- | Except clause in a try-except block
data ExceptClause = ExceptClause {
    exceptType :: Maybe Expr,
    exceptName :: Maybe String,
    exceptBody :: [AST]
} deriving (Show, Eq)

-- | Expression nodes cover literals, comprehensions, lambda, calls, attribute access, etc.
data Expr
    = Identifier String
    | IntLiteral Integer
    | FloatLiteral Double
    | StringLiteral String
    | BoolLiteral Bool
    | NoneLiteral
    | EllipsisLiteral
    | BinOp Expr BinOp Expr
    | UnaryOp UnaryOp Expr
    | Lambda [Param] Expr
    | DictLiteral [(Expr, Expr)]
    | SetLiteral [Expr]
    | ListLiteral [Expr]
    | TupleLiteral [Expr]
    | Comprehension Expr CompFor       -- e.g. [x for x in y]
    | Call Expr [CallArg]              -- func(args...)
    | Attribute Expr String            -- obj.attr
    | Subscript Expr Expr
    | Await Expr
    | Yield (Maybe Expr)
    | YieldFrom Expr
    | FormattedValue Expr (Maybe String) (Maybe Int) -- For f-strings
    | JoinedString [Expr]              -- For f-strings and concatenation
    deriving (Show, Eq)

data CallArg
    = Arg Expr
    | ArgKeyword String Expr
    | ArgUnpack Expr          -- *args
    | ArgKwUnpack Expr        -- **kwargs
    deriving (Show, Eq)

data CompFor = CompFor {
    compForTargets :: [Expr],
    compForIter    :: Expr,
    compForIfs     :: [Expr],
    compNestedFors :: [CompFor]
} deriving (Show, Eq)

-- | Operators
data BinOp
    = Add | Sub | Mult | Div | FloorDiv | Mod | Pow
    | LShift | RShift | BitOr | BitXor | BitAnd
    | MatMult | Lt | Gt | EqOp | NotEq | LtE | GtE
    | Is | IsNot | In | NotIn
    deriving (Show, Eq)

data UnaryOp
    = UAdd | USub | UNot | UInvert
    deriving (Show, Eq)

data AugOp
    = AugAdd | AugSub | AugMult | AugDiv | AugFloorDiv
    | AugMod | AugPow | AugLShift | AugRShift | AugBitOr
    | AugBitXor | AugBitAnd | AugMatMult
    deriving (Show, Eq)
-------------
-- Helpers --
-------------

trim :: String -> String
trim = trimStart . trimEnd

trimStart :: String -> String
trimStart = dropWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')

trimEnd :: String -> String
trimEnd = reverse . trimStart . reverse
