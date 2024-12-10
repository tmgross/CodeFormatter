module FormatTests where

import Test.HUnit
import System.IO (readFile, writeFile)
import System.Directory (removeFile)
import PythonFormatter (format) -- Assuming `format` is the function from PythonFormatter

-- File paths for the test
inputFile :: FilePath
inputFile = "test/test.py"

expectedFile :: FilePath
expectedFile = "test/test_expected.py"

outputFile :: FilePath
outputFile = "test/test_formatted.py"

-- | Test case to check if formatting produces the expected output
test_formatPythonFile :: Test
test_formatPythonFile = TestCase $ do
    -- Run the formatter to produce the formatted file
    content <- readFile inputFile
    formattedContent <- format content
    writeFile outputFile formattedContent
    
    -- Check that the output matches the expected result
    expectedContents <- readFile expectedFile
    formattedContents <- readFile outputFile
    assertEqual "Formatted output does not match expected output" expectedContents formattedContents

    -- Clean up the output file after the test
    removeFile outputFile

formatTests :: Test
formatTests = TestList [TestLabel "Testing Format" test_formatPythonFile]