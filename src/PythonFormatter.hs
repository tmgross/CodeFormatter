module PythonFormatter ( format ) where

-- A simple function that formats Python code (stubbed for now)
format :: String -> String
format code = unlines $ map fixIndentation (lines code)
    where
        fixIndentation line = "    " ++ line -- Just indent every line for now
