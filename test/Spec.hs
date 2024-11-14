-- Spec.hs
import Test.QuickCheck
import System.IO
import System.Directory (removeFile)
import PythonFormatter (format)

inputFile :: FilePath
inputFile = "test/test.py"

expectedFile :: FilePath
expectedFile = "test/test_expected.py"

outputFile :: FilePath
outputFile = "test/test_formatted.py"

-- | Property to test that formatting produces the expected output
prop_formatPythonFile :: Property
prop_formatPythonFile = ioProperty $ do
    -- Run the formatter to produce 'formatted_test.py'
    content <- readFile inputFile
    formattedContent <- format content
    writeFile outputFile formattedContent

    -- Check that the output matches the expected result
    result <- compareFiles expectedFile outputFile

    -- Clean up the output file after the test
    removeFile outputFile
    return result

-- | Helper function to compare the content of two files and return a Bool
compareFiles :: FilePath -> FilePath -> IO Bool
compareFiles expectedFile formattedFile = do
    expectedContents <- readFile expectedFile
    formattedContents <- readFile formattedFile
    return (expectedContents == formattedContents)

-- | Main function to run the property test
main :: IO ()
main = quickCheck prop_formatPythonFile
