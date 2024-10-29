module PythonLinter ( lint ) where

-- A simple function that lints Python code (stubbed for now)
lint :: String -> [String]
lint code = if "import" `elem` (words code)
              then ["Warning: Unused imports detected."]
              else []
