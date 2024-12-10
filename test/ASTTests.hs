-- test/ASTTests.hs
module ASTTests (astTests) where

import Test.HUnit
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import PythonParser
import PythonAST

-- Helper function to parse a string and assert the result
assertParse :: (Eq a, Show a) => Parser a -> String -> a -> Test
assertParse parser input expected = TestCase $ 
    case parse parser "" input of
        Left err -> assertFailure (show err)
        Right result -> assertEqual ("Parsing: " ++ input) expected result

-- Test cases for statements
testStatements :: Test
testStatements = TestList
    [ TestLabel "Stmt | Pass Statement" $ assertParse stmt "pass\n" PassStmt
    , TestLabel "Stmt | Break Statement" $ assertParse stmt "break\n" BreakStmt
    , TestLabel "Stmt | Continue Statement" $ assertParse stmt "continue\n" ContinueStmt
    , TestLabel "Stmt | Return Statement" $ assertParse stmt "return 42\n" (ReturnStmt (Just (IntLiteral 42)))
    , TestLabel "Stmt | Delete Statement" $ assertParse stmt "del x, y\n" (DeleteStmt [Identifier "x", Identifier "y"])
    , TestLabel "Stmt | Comment" $ assertParse stmt "# This is a comment\n" (Comment " This is a comment")
    , TestLabel "Stmt | Docstring" $ assertParse stmt "\"\"\"This is a docstring\"\"\"\n" (DocString "This is a docstring")
    , TestLabel "Stmt | Assignment" $ assertParse stmt "x = 42\n" (Assign [Identifier "x"] (IntLiteral 42))
    , TestLabel "Stmt | Augmented Assignment" $ assertParse stmt "x += 1\n" (AugAssign (Identifier "x") AugAdd (IntLiteral 1))
    , TestLabel "Stmt | Annotated Assignment" $ assertParse stmt "x: int = 42\n" (AnnAssign (Identifier "x") (Identifier "int") True)
    ]

-- Test cases for expressions
testExpressions :: Test
testExpressions = TestList
    [ TestLabel "Expr | Int Literal" $ assertParse expr "42" (IntLiteral 42)
    , TestLabel "Expr | Float Literal" $ assertParse expr "3.14" (FloatLiteral 3.14)
    , TestLabel "Expr | String Literal" $ assertParse expr "\"Hello\"" (StringLiteral "Hello")
    , TestLabel "Expr | Bool Literal" $ assertParse expr "True" (BoolLiteral True)
    , TestLabel "Expr | None Literal" $ assertParse expr "None" NoneLiteral
    , TestLabel "Expr | Unary Not" $ assertParse expr "not x" (UnaryOp UNot (Identifier "x"))
    , TestLabel "Expr | BinOp Addition" $ assertParse expr "x + y" (BinOp (Identifier "x") Add (Identifier "y"))
    , TestLabel "Expr | List Literal" $ assertParse expr "[1, 2, 3]" (ListLiteral [IntLiteral 1, IntLiteral 2, IntLiteral 3])
    , TestLabel "Expr | Dict Literal" $ assertParse expr "{\"key\": \"value\"}" 
        (DictLiteral [(StringLiteral "key", StringLiteral "value")])
    ]

-- Test cases for import statements
testImports :: Test
testImports = TestList
    [ TestLabel "Parser | Import | Single Module" $
        assertParse importStmt "import os\n" (ImportStmt [ImportItem "os" Nothing])
    , TestLabel "Parser | Import | Multiple Modules" $
        assertParse importStmt "import os, sys\n" 
            (ImportStmt [ImportItem "os" Nothing, ImportItem "sys" Nothing])
    , TestLabel "Parser | Import | With Alias" $
        assertParse importStmt "import numpy as np\n" 
            (ImportStmt [ImportItem "numpy" (Just "np")])
    , TestLabel "Parser | From Import | Single Item" $
        assertParse fromImportStmt "from math import sqrt\n" 
            (FromImportStmt "math" [ImportItem "sqrt" Nothing])
    , TestLabel "Parser | From Import | Multiple Items" $
        assertParse fromImportStmt "from math import sqrt, pi\n" 
            (FromImportStmt "math" [ImportItem "sqrt" Nothing, ImportItem "pi" Nothing])
    , TestLabel "Parser | From Import | With Alias" $
        assertParse fromImportStmt "from math import sqrt as root\n" 
            (FromImportStmt "math" [ImportItem "sqrt" (Just "root")])
    ]

-- Combine all tests into one
astTests :: Test
astTests = TestList
    [ TestLabel "Statements" testStatements
    , TestLabel "Expressions" testExpressions
    , TestLabel "Imports" testImports
    ]