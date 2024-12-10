module PythonLinter where

import Debug.Trace (trace)
import Data.Char (isSpace, isAlphaNum)
import Data.Maybe (mapMaybe)
import Data.List (nub, isPrefixOf)
import Control.Monad.State (State, get, put, execState, modify)

-- | Represents a single linting warning.
data LintWarning = LintWarning
  { warningType :: WarningType
  , lineNumber  :: Int
  , message     :: String
  } deriving (Show, Eq)

-- | Enum for different types of warnings.
data WarningType
  = UnusedImport
  | SyntaxError
  | NamingConventionViolation
  | DeprecatedFunction
  | MutableDefaultArgument
  deriving (Show, Eq)

-- | Represents the state used during the linting process.
data LintState = LintState
  { warnings     :: [LintWarning]
  , errorCount   :: Int
  , warningCount :: Int
  , summary      :: String
  } deriving (Show, Eq)

-- | Initial state for the linting process.
initialLintState :: LintState
initialLintState = LintState
  { warnings = []
  , errorCount = 0
  , warningCount = 0
  , summary = "Starting linting..."
  }

-- | Lints Python code by calling various checks and returning the final `LintState`.
lint :: String -> LintState
lint code = execState (runLintChecks code) initialLintState

-- | Run all lint checks, updating the state for each warning found.
runLintChecks :: String -> State LintState ()
runLintChecks code = do
    addWarnings $ checkUnusedImports code
    -- addWarnings $ checkSyntaxErrors code
    -- addWarnings $ validateNamingConventions code
    -- addWarnings $ checkUnusedVariables code
    -- addWarnings $ checkDeprecatedFunctions code
    -- addWarnings $ checkMutableDefaults code
    updateSummary

-- | Adds warnings to the state, updating error and warning counts.
addWarnings :: [LintWarning] -> State LintState ()
addWarnings ws = do
    st <- get
    let (e, w) = countSeverity ws
    put st { warnings = warnings st ++ ws
           , errorCount = errorCount st + e
           , warningCount = warningCount st + w
           }

-- | Update the summary based on the current state.
updateSummary :: State LintState ()
updateSummary = do
    st <- get
    let newSummary = "Linting completed with " ++ show (errorCount st)
                     ++ " errors and " ++ show (warningCount st) ++ " warnings."
    put st { summary = newSummary }

-- | Helper function to count errors and warnings based on WarningType
countSeverity :: [LintWarning] -> (Int, Int)
countSeverity = foldr (\w (eCount, wCount) ->
            case warningType w of
              SyntaxError -> (eCount + 1, wCount)
              _           -> (eCount, wCount + 1)
          ) (0, 0)

-- | Generate a LintWarning for an unused import.
generateWarning :: [Int] -> String -> LintWarning
generateWarning lines importLine =
    let lineNum = maybe 0 id (lookup importLine (zip [importLine] lines))
    in LintWarning
        { warningType = UnusedImport
        , lineNumber = lineNum
        , message = "Unused import: " ++ importLine
        }

-- | Checks for unused imports in Python code.
checkUnusedImports :: String -> [LintWarning]
checkUnusedImports code =
    let linesOfCode = zip [1..] (lines code)
        imports = findImports linesOfCode
        -- trace ("Lines of code: " ++ show linesOfCode) $
        -- _ = trace ("Detected imports: " ++ show imports) ()
        usedSymbols = findUsedSymbols code
        -- trace ("Code being analyzed for symbols: " ++ code ++ "imports: " ++ show imports) $
        -- _ = trace ("Detected used symbols: " ++ show usedSymbols) ()
        unused = filter (`notElem` usedSymbols) (map snd imports)
        -- _ = trace ("Unused imports detected: " ++ show unused) ()
    in map (generateWarning (map fst imports)) unused

-- | Find all imports and their line numbers, returning only the module names.
findImports :: [(Int, String)] -> [(Int, String)]
findImports = mapMaybe extractImport
  where
    extractImport (lineNum, line) =
        let trimmed = dropWhile isSpace line
        in if "import " `isPrefixOf` trimmed
              then Just (lineNum, extractModuleName (drop (length "import ") trimmed))
              else if "from " `isPrefixOf` trimmed
                  then case words (drop (length "from ") trimmed) of
                      (modName : "import" : _) -> Just (lineNum, modName)
                      _ -> Nothing
                  else Nothing

    -- Extracts the module name from an "import <module>" line
    extractModuleName = takeWhile (`notElem` " .,;(){}[]")

-- | Find all symbols that appear to be used in the code, excluding those in import statements.
findUsedSymbols :: String -> [String]
findUsedSymbols code =
    let linesWithoutImports = filter (not . isImport) (lines code)
        tokens = splitSymbols (unlines linesWithoutImports)
        symbols = filter isSymbol tokens
    in nub symbols
  where
    isSymbol s = not (null s) && all isAlphaNum s

    -- Checks if a line is an import statement
    isImport line =
        let trimmed = dropWhile isSpace line
        in "import " `isPrefixOf` trimmed || "from " `isPrefixOf` trimmed

-- | Splits a string based on the delimiters " .,;(){}[]"
splitSymbols :: String -> [String]
splitSymbols = words . map (\c -> if c `elem` " .,;(){}[]" then ' ' else c)

-- | Analyzes Python code for syntax errors.
checkSyntaxErrors :: String -> [LintWarning]
checkSyntaxErrors = undefined

-- | Validates the naming conventions in the Python code.
validateNamingConventions :: String -> [LintWarning]
validateNamingConventions = undefined

-- | Checks for unused variable declarations.
checkUnusedVariables :: String -> [LintWarning]
checkUnusedVariables = undefined

-- | Checks for deprecated function usage.
checkDeprecatedFunctions :: String -> [LintWarning]
checkDeprecatedFunctions = undefined

-- | Checks for mutable default arguments in function definitions.
checkMutableDefaults :: String -> [LintWarning]
checkMutableDefaults = undefined
