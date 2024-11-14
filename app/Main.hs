module Main where

import System.FilePath (takeExtension, dropExtension)
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
            writeFile (dropExtension filePath ++ "_formatted" ++ fileExt) formatted
            putStrLn "File formatted successfully!"
            
            -- Extract warnings and print them
            let lintWarnings = warnings lintResult
            mapM_ (putStrLn . formatWarning) lintWarnings  -- Print each warning
        _ -> putStrLn $ "Unsupported file type: " ++ fileExt

-- | Format a LintWarning for printing
formatWarning :: PythonLinter.LintWarning -> String
formatWarning (PythonLinter.LintWarning _ lineNumber message) =
    "Line " ++ show lineNumber ++ ": " ++ message
