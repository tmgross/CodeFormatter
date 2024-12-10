module LintTests where

import Test.HUnit
import System.IO (readFile, writeFile)
import System.Directory (removeFile)
import PythonLinter (lint, warnings, LintWarning(..))
import Data.List (intercalate)

-- | File paths for the test
testFile :: FilePath
testFile = "test/testLints.py"

outputLintFile :: FilePath
outputLintFile = "test/testLints_lint.txt"

expectedLintFile :: FilePath
expectedLintFile = "test/testLints_expectedLint.txt"

-- | Converts a list of LintWarning to a string format for comparison
formatWarnings :: [LintWarning] -> String
formatWarnings ws =
    intercalate "\n" $ map formatWarning ws
  where
    formatWarning (LintWarning wType lineNum msg) =
        "Line " ++ show lineNum ++ " [" ++ show wType ++ "]: " ++ msg

-- | Test case to check if linting produces the expected output
test_linter :: Test
test_linter = TestCase $ do
    -- Read the test file
    code <- readFile testFile
    
    -- Run the linter and get the warnings
    let lintResult = lint code
    let lintWarnings = warnings lintResult
    
    -- Write the generated warnings to the output file
    let formattedWarnings = formatWarnings lintWarnings
    writeFile outputLintFile formattedWarnings
    
    -- Read the expected lint warnings
    expectedWarnings <- readFile expectedLintFile
    
    -- Compare the generated warnings with the expected output
    assertEqual "Lint output does not match expected output" expectedWarnings formattedWarnings

    -- Clean up the output file after the test
    removeFile outputLintFile
    removeFile "test/testLints_formatted.py"

lintTests :: Test
lintTests = TestList [TestLabel "test_linter" test_linter]