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
    [ assertParse stmt "pass\n" PassStmt
    , assertParse stmt "break\n" BreakStmt
    , assertParse stmt "continue\n" ContinueStmt
    , assertParse stmt "return 42\n" (ReturnStmt (Just (IntLiteral 42)))
    , assertParse stmt "del x, y\n" (DeleteStmt [Identifier "x", Identifier "y"])
    , assertParse stmt "# This is a comment\n" (Comment " This is a comment")
    , assertParse stmt "\"\"\"This is a docstring\"\"\"\n" (DocString "This is a docstring")
    , assertParse stmt "x = 42\n" (Assign [Identifier "x"] (IntLiteral 42))
    , assertParse stmt "x += 1\n" (AugAssign (Identifier "x") AugAdd (IntLiteral 1))
    , assertParse stmt "x: int = 42\n" (AnnAssign (Identifier "x") (Identifier "int") True)
    ]

-- Test cases for expressions
testExpressions :: Test
testExpressions = TestList
    [ assertParse expr "42" (IntLiteral 42)
    , assertParse expr "3.14" (FloatLiteral 3.14)
    , assertParse expr "\"Hello\"" (StringLiteral "Hello")
    , assertParse expr "True" (BoolLiteral True)
    , assertParse expr "None" NoneLiteral
    , assertParse expr "x + y" (BinOp (Identifier "x") Add (Identifier "y"))
    , assertParse expr "x if y > 0 else z" 
        (IfExp (Identifier "x") 
            (BinOp (Identifier "y") Gt (IntLiteral 0)) 
            (Identifier "z"))
    , assertParse expr "[1, 2, 3]" (ListLiteral [IntLiteral 1, IntLiteral 2, IntLiteral 3])
    , assertParse expr "{'key': 'value'}" 
        (DictLiteral [(StringLiteral "key", StringLiteral "value")])
    ]

-- Combine all tests into one
astTests :: Test
astTests = TestList
    [ TestLabel "Statements" testStatements
    , TestLabel "Expressions" testExpressions
    ]
