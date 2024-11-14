module PythonFormatter (format, Config(..)) where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import System.IO
import System.Directory (doesFileExist)

-- | Configuration for formatting options
data Config = Config
  { tabWidth         :: Int
  , maxLineLength    :: Maybe Int
  , trimTrailingWS   :: Bool
  , addNewlineEOF    :: Bool
  } deriving (Show, Eq)

-- | Read configuration from a file, defaulting if not found or parse error
loadConfig :: IO Config
loadConfig = do
    configExists <- doesFileExist "formatter.config"
    if configExists
    then do
        content <- readFile "formatter.config"
        let settings = parseConfig content
        return Config {
            tabWidth = fromMaybe 4 (Map.lookup "tabWidth" settings >>= readMaybe),
            maxLineLength = Map.lookup "maxLineLength" settings >>= readMaybe,
            trimTrailingWS = fromMaybe True (Map.lookup "trimTrailingWS" settings >>= readBool),
            addNewlineEOF = fromMaybe True (Map.lookup "addNewlineEOF" settings >>= readBool)
        }
    else return $ Config 4 Nothing True True -- default configuration values

-- | Parse configuration key-value pairs
parseConfig :: String -> Map.Map String String
parseConfig = Map.fromList . map (break (== '=') . trim) . lines

-- | Read integer values safely
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(val, "")] -> Just val
    _           -> Nothing

-- | Read boolean values safely
readBool :: String -> Maybe Bool
readBool "true"  = Just True
readBool "false" = Just False
readBool _       = Nothing

-- | Format Python code with loaded configuration
format :: String -> IO String
format code = do
    config <- loadConfig
    let formattedLines = formatLines (lines code) config 0 []
    return $ unlines formattedLines

-- | Main function to format each line according to indentation and settings
formatLines :: [String] -> Config -> Int -> [String] -> [String]
formatLines = undefined

-- | Trim whitespace from both ends of a string
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
