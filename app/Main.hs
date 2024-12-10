module Main where

import System.FilePath (takeExtension, dropExtension, (</>))
import System.IO (readFile, writeFile)
import System.Environment (getArgs)
import PythonFormatter
import PythonLinter

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> processFile filePath
        _ -> putStrLn "Usage: stack run <file-path>"

-- | Process a file by detecting its extension and applying the correct formatter and linter
processFile :: FilePath -> IO ()
processFile filePath = do
    let fileExt = takeExtension filePath
    contents <- readFile filePath
    case fileExt of
        ".py" -> do
            -- Format the Python code
            formatted <- PythonFormatter.format contents
            
            -- Lint the Python code and get the result
            let lintResult = PythonLinter.lint contents
            
            -- Write the formatted content to a new file
            let formattedFilePath = dropExtension filePath ++ "_formatted" ++ fileExt
            writeFile formattedFilePath formatted
            putStrLn $ "File formatted successfully! Written to: " ++ formattedFilePath
            
            -- Extract warnings and write them to a new file
            let lintWarnings = warnings lintResult
            let lintFilePath = dropExtension filePath ++ "_lint.txt"
            let formattedWarnings = unlines $ map formatWarning lintWarnings
            writeFile lintFilePath formattedWarnings
            putStrLn $ "Lint warnings written to: " ++ lintFilePath
        _ -> putStrLn $ "Unsupported file type: " ++ fileExt

-- | Format a LintWarning for printing
formatWarning :: PythonLinter.LintWarning -> String
formatWarning (PythonLinter.LintWarning _ lineNumber message) =
    "Line " ++ show lineNumber ++ ": " ++ message
