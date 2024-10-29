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

-- Process a file by detecting its extension and applying the correct formatter and linter
processFile :: FilePath -> IO ()
processFile filePath = do
    let fileExt = takeExtension filePath
    contents <- readFile filePath
    let (formatted, lintWarnings) = case fileExt of
            ".py"   -> (PythonFormatter.format contents, PythonLinter.lint contents)
            -- ".hs"   -> (Haskell.format contents, HaskellLinter.lint contents)
            -- ".json" -> (JSON.format contents, JSONLinter.lint contents)
            _       -> (contents, ["Unsupported file type: " ++ fileExt])
    writeFile (dropExtension filePath ++ "_new" ++ fileExt) formatted
    putStrLn "File formatted successfully!"
    mapM_ putStrLn lintWarnings
