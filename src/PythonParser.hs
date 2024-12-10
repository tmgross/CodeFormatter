{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module PythonParser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import PythonAST
import Data.Functor (void)

------------------------------
-- Top-Level Parser
------------------------------

program :: Parser AST
program = do
    -- Consume initial whitespace/comments as needed
    -- Parse multiple top-level statements
    stmts <- many topLevelStatement
    eof
    return (Module stmts)

topLevelStatement :: Parser AST
topLevelStatement = stmt

------------------------------
-- Statements
------------------------------

stmt :: Parser AST
stmt = try compoundStmt <|> simpleStmt <* lineEnd

simpleStmt :: Parser AST
simpleStmt = choice
    [ try returnStmt
    , try raiseStmt
    , try passStmt
    , try breakStmt
    , try continueStmt
    , try globalStmt
    , try nonlocalStmt
    , try deleteStmt
    , try docStringStmt
    , try assignStmt
    , try augAssignStmt
    , try annAssignStmt
    , try exprStmt
    , try commentStmt
    ]

compoundStmt :: Parser AST
compoundStmt = choice
    [ ifStmt
    , forStmt
    , asyncForStmt
    , whileStmt
    , tryStmt
    , withStmt
    , asyncWithStmt
    , classDefStmt
    , functionDefStmt
    , asyncFunctionDefStmt
    , importStmt
    , fromImportStmt
    ]

importStmt :: Parser AST
importStmt = do
  string "import"
  whitespace
  modules <- sepBy importItem (char ',' >> whitespace)
  return $ ImportStmt modules

fromImportStmt :: Parser AST
fromImportStmt = do
  string "from"
  whitespace
  moduleName <- identifier
  whitespace
  string "import"
  whitespace
  items <- sepBy importItem (char ',' >> whitespace)
  return $ FromImportStmt moduleName items

classDefStmt :: Parser AST
classDefStmt = do
    string "class"
    whitespace
    name <- identifier
    bases <- option [] (try parseBases)
    whitespace
    char ':'
    lineEnd
    body <- indentedBlock stmt
    return $ ClassDef name bases [] body

-- Parses base classes in parentheses, e.g., `(Base1, Base2)`
parseBases :: Parser [Expr]
parseBases = do
    char '('
    bases <- sepBy expr (char ',' >> whitespace)
    char ')'
    return bases

functionDefStmt :: Parser AST
functionDefStmt = do
    string "def"
    whitespace
    name <- identifier
    whitespace
    char '('
    ps <- params
    char ')'
    returnType <- optionMaybe parseReturnType
    whitespace
    char ':'
    lineEnd
    body <- indentedBlock stmt
    return $ FunctionDef name ps returnType body

-- Parses the return type hint after `->`
parseReturnType :: Parser Expr
parseReturnType = do
    whitespace
    string "->"
    whitespace
    expr

asyncFunctionDefStmt :: Parser AST
asyncFunctionDefStmt = do
    string "async"
    whitespace
    string "def"
    whitespace
    name <- identifier
    whitespace
    char '('
    ps <- params
    char ')'
    returnType <- optionMaybe parseReturnType
    whitespace
    char ':'
    lineEnd
    body <- indentedBlock stmt
    return $ AsyncFunctionDef name ps returnType body

ifStmt :: Parser AST
ifStmt = do
    string "if"
    whitespace
    condition <- expr
    whitespace
    char ':'
    lineEnd
    ifBody <- indentedBlock stmt
    elifClauses <- many elifStmt
    elseBody <- option [] elseStmt
    return $ IfStmt condition ifBody elifClauses elseBody

elifStmt :: Parser (Expr, [AST])
elifStmt = do
    string "elif"
    whitespace
    condition <- expr
    whitespace
    char ':'
    lineEnd
    body <- indentedBlock stmt
    return (condition, body)

elseStmt :: Parser [AST]
elseStmt = do
    string "else"
    whitespace
    char ':'
    lineEnd
    indentedBlock stmt

forStmt :: Parser AST
forStmt = do
    string "for"
    whitespace
    target <- expr
    whitespace
    string "in"
    whitespace
    iterable <- expr
    whitespace
    char ':'
    lineEnd
    body <- indentedBlock stmt
    elseBody <- option [] elseStmt
    return $ ForStmt target iterable body elseBody

asyncForStmt :: Parser AST
asyncForStmt = do
    string "async"
    whitespace
    string "for"
    whitespace
    target <- expr
    whitespace
    string "in"
    whitespace
    iterable <- expr
    whitespace
    char ':'
    lineEnd
    body <- indentedBlock stmt
    elseBody <- option [] elseStmt
    return $ AsyncForStmt target iterable body elseBody

whileStmt :: Parser AST
whileStmt = do
    string "while"
    whitespace
    condition <- expr
    whitespace
    char ':'
    lineEnd
    body <- indentedBlock stmt
    elseBody <- option [] elseStmt
    return $ WhileStmt condition body elseBody

tryStmt :: Parser AST
tryStmt = do
    string "try"
    whitespace
    char ':'
    lineEnd
    tryBody <- indentedBlock stmt
    exceptClauses <- many exceptStmt
    elseBody <- option [] elseStmt
    finallyBody <- option [] finallyStmt
    return $ TryStmt tryBody exceptClauses elseBody finallyBody

exceptStmt :: Parser ExceptClause
exceptStmt = do
    string "except"
    whitespace
    exceptType <- optionMaybe (try expr)
    exceptName <- optionMaybe (try (whitespace >> string "as" >> whitespace >> identifier))
    whitespace
    char ':'
    lineEnd
    exceptBody <- indentedBlock stmt
    return $ ExceptClause exceptType exceptName exceptBody

finallyStmt :: Parser [AST]
finallyStmt = do
    string "finally"
    whitespace
    char ':'
    lineEnd
    indentedBlock stmt

withStmt :: Parser AST
withStmt = do
    string "with"
    whitespace
    items <- sepBy withItem (char ',' >> whitespace)
    whitespace
    char ':'
    lineEnd
    body <- indentedBlock stmt
    return $ WithStmt items body

withItem :: Parser (Expr, Maybe String)
withItem = do
    contextExpr <- expr
    alias <- optionMaybe (try (whitespace >> string "as" >> whitespace >> identifier))
    return (contextExpr, alias)

asyncWithStmt :: Parser AST
asyncWithStmt = do
    string "async"
    whitespace
    string "with"
    whitespace
    items <- sepBy withItem (char ',' >> whitespace)
    whitespace
    char ':'
    lineEnd
    body <- indentedBlock stmt
    return $ AsyncWithStmt items body

returnStmt :: Parser AST
returnStmt = do
    string "return"
    whitespace
    value <- optionMaybe expr
    lineEnd
    return $ ReturnStmt value

raiseStmt :: Parser AST
raiseStmt = do
    string "raise"
    whitespace
    exception <- optionMaybe expr
    fromClause <- optionMaybe (try parseFromClause)
    lineEnd
    return $ RaiseStmt exception fromClause

parseFromClause :: Parser Expr
parseFromClause = do
    whitespace
    string "from"
    whitespace
    expr

passStmt :: Parser AST
passStmt = do
    string "pass"
    whitespace
    lineEnd
    return PassStmt

breakStmt :: Parser AST
breakStmt = do
    string "break"
    whitespace
    lineEnd
    return BreakStmt

continueStmt :: Parser AST
continueStmt = do
    string "continue"
    whitespace
    lineEnd
    return ContinueStmt

globalStmt :: Parser AST
globalStmt = do
    string "global"
    whitespace
    variables <- sepBy1 identifier (char ',' >> whitespace)
    lineEnd
    return $ GlobalStmt variables

nonlocalStmt :: Parser AST
nonlocalStmt = do
    string "nonlocal"
    whitespace
    variables <- sepBy1 identifier (char ',' >> whitespace)
    lineEnd
    return $ NonlocalStmt variables

exprStmt :: Parser AST
exprStmt = do
    expression <- expr
    lineEnd
    return $ ExprStmt expression

assignStmt :: Parser AST
assignStmt = do
    targets <- sepBy1 expr (whitespace >> char '=' >> whitespace)
    let value = last targets
    let vars = init targets
    if length targets == 1
        then fail "Invalid assignment: no value provided"
        else return $ Assign vars value

augAssignStmt :: Parser AST
augAssignStmt = do
    target <- identifierLiteral
    whitespace
    operator <- parseAugOp
    whitespace
    value <- expr
    lineEnd
    return $ AugAssign target operator value

annAssignStmt :: Parser AST
annAssignStmt = do
    target <- expr
    whitespace
    char ':'
    whitespace
    annotation <- expr
    hasValue <- option False (True <$ parseAssignValue)
    lineEnd
    return $ AnnAssign target annotation hasValue

-- Parses the assignment part of an annotated assignment (e.g., `= 3.14`).
parseAssignValue :: Parser ()
parseAssignValue = do
    whitespace
    char '='
    whitespace
    expr
    return ()

deleteStmt :: Parser AST
deleteStmt = do
    string "del"
    whitespace
    targets <- sepBy1 expr (char ',' >> whitespace)
    lineEnd
    return $ DeleteStmt targets

docStringStmt :: Parser AST
docStringStmt = do
    tripleQuote <- string "\"\"\"" <|> string "'''"
    content <- manyTill anyChar (try (string tripleQuote))
    lineEnd
    return $ DocString content

commentStmt :: Parser AST
commentStmt = do
    char '#'
    content <- many (noneOf "\n")
    optional lineEnd
    return $ Comment content

------------------------------
-- Expressions
------------------------------

-- Expression parser with operators
expr :: Parser Expr
expr = primary

-- Parses the most basic expressions, such as literals, if expressions, or parenthesized expressions.
primary :: Parser Expr
primary = choice
    [ try callExpr
    , try attributeRef
    , try subscriptExpr
    , atom
    ]

-- Matches atomic expressions like literals, tuples, lists, dictionaries, and sets.
atom :: Parser Expr
atom = choice
    [ try floatLiteral
    , try intLiteral
    , try stringLiteral
    , try boolLiteral
    , try noneLiteral
    , try ellipsisLiteral
    , try binaryExpr
    , try unaryExpr
    , try identifierLiteral
    , try tupleLiteral
    , try listLiteral
    , try dictLiteral
    , try setLiteral
    , try (parens expr)
    ]

binaryExpr :: Parser Expr
binaryExpr = do
    e1 <- identifierLiteral
    whitespace
    op <- parseBinOp
    whitespace
    e2 <- identifierLiteral
    return $ BinOp e1 op e2

unaryExpr :: Parser Expr
unaryExpr = do
    op <- parseUnaryOp
    whitespace
    e <- atom
    return $ UnaryOp op e

identifierLiteral :: Parser Expr
identifierLiteral = do
    name <- identifier
    return $ Identifier name

-- Matches an integer literal.
-- Example: "42"
intLiteral :: Parser Expr
intLiteral = IntLiteral <$> (read <$> many1 digit)

-- Matches a floating-point literal.
-- Example: "3.14"
floatLiteral :: Parser Expr
floatLiteral = do
    whole <- many1 digit
    char '.'
    fractional <- many1 digit
    return $ FloatLiteral (read (whole ++ "." ++ fractional))

-- Matches a string literal enclosed in double quotes.
-- Example: "\"Hello, world!\""
stringLiteral :: Parser Expr
stringLiteral = do
    char '"'
    content <- many (noneOf "\"")
    char '"'
    return $ StringLiteral content

-- Matches a boolean literal ("True" or "False").
-- Examples: "True", "False"
boolLiteral :: Parser Expr
boolLiteral = (BoolLiteral True <$ string "True") <|> (BoolLiteral False <$ string "False")

-- Matches the Python "None" literal.
-- Example: "None"
noneLiteral :: Parser Expr
noneLiteral = NoneLiteral <$ string "None"

-- Matches the Python ellipsis literal ("...").
-- Example: "..."
ellipsisLiteral :: Parser Expr
ellipsisLiteral = EllipsisLiteral <$ string "..."

-- Matches a dictionary literal.
-- Example: "{'key': 'value', 'other': 42}"
dictLiteral :: Parser Expr
dictLiteral = do
    char '{'
    pairs <- sepBy keyValuePair (char ',' >> whitespace)
    char '}'
    return $ DictLiteral pairs
  where
    keyValuePair = do
        key <- expr
        whitespace
        char ':'
        whitespace
        value <- expr
        return (key, value)

-- Matches a set literal.
-- Example: "{1, 2, 3}"
setLiteral :: Parser Expr
setLiteral = do
    char '{'
    elements <- sepBy expr (char ',' >> whitespace)
    char '}'
    return $ SetLiteral elements

-- Matches a list literal.
-- Example: "[1, 2, 3]"
listLiteral :: Parser Expr
listLiteral = do
    char '['
    elements <- sepBy expr (char ',' >> whitespace)
    char ']'
    return $ ListLiteral elements

-- Matches a tuple literal.
-- Example: "(1, 2, 3)"
tupleLiteral :: Parser Expr
tupleLiteral = do
    char '('
    elements <- sepBy expr (char ',' >> whitespace)
    char ')'
    return $ TupleLiteral elements

-- Matches a function call.
-- Example: "func(1, 2, 3)"
callExpr :: Parser Expr
callExpr = do
    func <- atom
    char '('
    args <- sepBy expr (char ',' >> whitespace)
    char ')'
    return $ Call func (map Arg args)

-- Matches attribute access.
-- Example: "obj.attr"
attributeRef :: Parser Expr
attributeRef = do
    obj <- atom
    char '.'
    attr <- identifier
    return $ Attribute obj attr

-- Matches subscripting or indexing.
-- Example: "arr[0]"
subscriptExpr :: Parser Expr
subscriptExpr = do
    obj <- atom
    char '['
    subscript <- expr
    char ']'
    return $ Subscript obj subscript

-- Matches an "await" expression for asynchronous code.
-- Example: "await some_async_func()"
awaitExpr :: Parser Expr
awaitExpr = Await <$> (string "await" >> whitespace >> expr)

-- Matches a "yield" or "yield from" expression.
-- Examples:
-- "yield x"
-- "yield from iterable"
yieldExpr :: Parser Expr
yieldExpr = try (YieldFrom <$> (string "yield from" >> whitespace >> expr))
         <|> (Yield <$> optionMaybe (string "yield" >> whitespace >> expr))

-- Matches a formatted string (f-string).
-- Example: "f\"Hello, {name}!\""
fString :: Parser Expr
fString = do
    char 'f'
    char '"'
    content <- many (formattedValue <|> rawText)
    char '"'
    return $ JoinedString content
  where
    formattedValue = do
        char '{'
        value <- expr
        char '}'
        return $ FormattedValue value Nothing Nothing
    rawText = StringLiteral <$> many1 (noneOf "{}")

------------------------------
-- Operators
------------------------------

-- | Parser for binary operators.
-- This parser tries the longest matches first (like "**") before shorter ones.
parseBinOp :: Parser BinOp
parseBinOp = choice
    [ try (string "**")   >> return Pow
    , try (string "//")   >> return FloorDiv
    , try (string "<<")   >> return LShift
    , try (string ">>")   >> return RShift
    , string "+"          >> return Add
    , string "-"          >> return Sub
    , string "*"          >> return Mult
    , string "@"          >> return MatMult
    , string "/"          >> return Div
    , string "%"          >> return Mod
    , string "|"          >> return BitOr
    , string "^"          >> return BitXor
    , string "&"          >> return BitAnd
    , try (string "==")   >> return EqOp
    , try (string "!=")   >> return NotEq
    , try (string "<=")   >> return LtE
    , try (string ">=")   >> return GtE
    , string "<"          >> return Lt
    , string ">"          >> return Gt
    , try (string "is not") >> return IsNot
    , string "is"         >> return Is
    , try (string "not in") >> return NotIn
    , string "in"         >> return In
    ]

-- | Parser for unary operators.
parseUnaryOp :: Parser UnaryOp
parseUnaryOp = choice
    [ string "not" >> notFollowedBy alphaNum >> return UNot
    , char '~' >> return UInvert
    , char '+' >> return UAdd
    , char '-' >> return USub
    ]

-- | Parser for augmented assignment operators.
parseAugOp :: Parser AugOp
parseAugOp = choice
    [ try (string "+=")  >> return AugAdd
    , try (string "-=")  >> return AugSub
    , try (string "*=")  >> return AugMult
    , try (string "@=")  >> return AugMatMult
    , try (string "/=")  >> return AugDiv
    , try (string "%=")  >> return AugMod
    , try (string "//=") >> return AugFloorDiv
    , try (string "**=") >> return AugPow
    , try (string "<<=") >> return AugLShift
    , try (string ">>=") >> return AugRShift
    , try (string "|=")  >> return AugBitOr
    , try (string "^=")  >> return AugBitXor
    , try (string "&=")  >> return AugBitAnd
    ]

------------------------------
-- Parameters and Arguments
------------------------------

-- | Parser for single parameter
param :: Parser Param
param = do
    paramN  <- identifier
    paramTH <- optionMaybe (try (whitespace >> char ':' >> whitespace >> expr))
    paramD  <- optionMaybe (try (whitespace >> char '=' >> whitespace >> expr))
    return $ Param paramN paramTH paramD

-- | Parser for multiple parameters
params :: Parser [Param]
params = sepBy param (whitespace >> char ',' >> whitespace)

------------------------------
-- Imports
------------------------------

importItem :: Parser ImportItem
importItem = do
  moduleName <- identifier
  alias <- optionMaybe (try (whitespace >> string "as" >> whitespace >> identifier))
  return $ ImportItem moduleName alias

------------------------------
-- Indentation and Whitespace
------------------------------

indentation :: Parser Int
indentation = do
    spacesOrTabs <- many (oneOf " \t")
    if all (== ' ') spacesOrTabs || all (== '\t') spacesOrTabs
        then return (length spacesOrTabs)
        else fail "Mixed whitespace and tabs for indentation"

-- | Parses a block of indented statements using the given statement parser.
indentedBlock :: Parser AST -> Parser [AST]
indentedBlock stmtParser = do
    parentIndent <- currentIndentation  -- Get the current parent indentation level
    many1 (indentedStmt parentIndent stmtParser) -- Parse statements with greater indentation

-- | Parses a single statement with the expected indentation level.
indentedStmt :: Int -> Parser AST -> Parser AST
indentedStmt parentIndent stmtParser = do
    actualIndent <- currentIndentation
    if actualIndent > parentIndent
        then stmtParser -- Parse the statement if indentation is valid
        else fail "Unexpected indentation level"

-- | Parses the current line's indentation and returns its level.
currentIndentation :: Parser Int
currentIndentation = do
    spacesOrTabs <- many (oneOf " \t")  -- Consume leading whitespace or tabs
    let indentLevel = length spacesOrTabs
    return indentLevel

------------------------------
-- Helpers
------------------------------

identifier :: Parser String
identifier = do
  first <- letter
  rest <- many (alphaNum <|> char '_')
  return (first:rest)

parens :: Parser a -> Parser a
parens p = do
    char '('
    whitespace
    x <- p
    whitespace
    char ')'
    return x

lineEnd :: Parser ()
lineEnd = void (newline) <|> eof

whitespace :: Parser ()
whitespace = void $ many (char ' ' <|> char '\t') -- Match whitespace/tabs