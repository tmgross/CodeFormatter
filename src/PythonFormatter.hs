module PythonFormatter where

import Control.Monad.State (State, get, put, runState)
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

-- | The state that tracks the formatting status
data FormatState = FormatState
  { config         :: Config
  , indentLevel    :: Int
  , blockStack     :: [String]
  , formattedLines :: [String]
  }

-- | Default FormatState, using default config values
defaultFormatState :: Config -> FormatState
defaultFormatState config = FormatState
  { config = config
  , indentLevel = 0
  , blockStack = []
  , formattedLines = []                  
  }

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
    let initialState = defaultFormatState config
    let (formattedCode, finalState) = runState (formatLines (lines code)) initialState
    return $ unlines formattedCode

-- | Format each line with updated state, handling indentation and block structure
formatLines :: [String] -> State FormatState [String]
formatLines = undefined

-- | Helper to determine if a line starts a new block (e.g., ends with a colon)
isBlockStart :: String -> Bool
isBlockStart line = not (null line) && last line == ':'

-- | Helper to determine if the line should end a block (used for dedenting)
isBlockEnd :: String -> [String] -> Bool
isBlockEnd line blockStack = 
    case blockStack of
        (top:_) -> any (`elem` endKeywords) (words line) && top `notElem` endKeywords
        [] -> False
  where
    endKeywords = ["return", "break", "continue", "pass", "else:", "elif:"]

-- | Helper to add indentation spaces
indentLine :: String -> Int -> String
indentLine line n = replicate n ' ' ++ line

-- | Trim whitespace from both ends of a string
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace