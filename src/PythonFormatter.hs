module PythonFormatter where

import Control.Monad.State (State, get, put, runState)
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import System.IO
import System.Directory (doesFileExist)

import Debug.Trace (trace)

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
    let processedCode = if addNewlineEOF config
                        then unlines formattedCode ++ "\n"
                        else unlines formattedCode
    return processedCode

-- | Format each line with updated state, handling indentation and block structure
formatLines :: [String] -> State FormatState [String]
formatLines [] = return []
formatLines (line:rest) = do
    state <- get
    let _line = process (indentLine line (indentLevel state))
    let _state = updateState _line state
    put _state
    _rest <- formatLines rest
    trace (show (indentLevel _state) ++ ": " ++ _line)  return (_line:_rest)

updateState :: String -> FormatState -> FormatState
updateState line state =
    let
        currentIndent = indentLevel state
        currentBlockStack = blockStack state

        isBlockStart = not (null line) && last line == ':'
        newBlockStack
            | isBlockStart = line : currentBlockStack
            | isElse line currentBlockStack = currentBlockStack  
            | otherwise = currentBlockStack

        isClass = "class" `isInfixOf` line
        isDef = "def" `isInfixOf` line

        indent
            | isClass = 1
            | isBlockStart = currentIndent + 1 
            | isBlockEnd line currentBlockStack = max 0 (currentIndent - 1) 
            | otherwise = currentIndent

    in
    state { indentLevel = indent, blockStack = newBlockStack }

process :: String -> String
process [] = []
process [c] = [c]
process (c1:c2:rest)
    -- operators
    | c1 `elem` "+-*/<>=!" && not (isSpace c2) = c1 : ' ' : process (c2:rest)
    | c2 `elem` "+-*/<>=!" && not (isSpace c1) = c1 : ' ' : process (c2:rest)
    -- comma spacing
    | c1 == ',' && not (isSpace c2) = c1 : ' ' : process (c2:rest)
    | otherwise = c1 : process (c2:rest)

-- | Format a Python dictionary literal.
formatDict :: String -> Int -> State FormatState String
formatDict = undefined

-- | Format a Python list literal.
formatList :: String -> Int -> State FormatState String
formatList listString indentLevel = undefined

-- | Format a Python function definition, including indentation and spacing.
formatFunction :: String -> Int -> State FormatState String
formatFunction funcString indentLevel = undefined

-- | Format Python if-else structures.
formatIfElse :: String -> Int -> State FormatState String
formatIfElse ifElseString indentLevel = undefined

-- | Format Python class definitions.
formatClass :: String -> Int -> State FormatState String
formatClass classString indentLevel = undefined

-- | Split long lines into smaller chunks while respecting Python syntax.
splitLongLines :: String -> Int -> State FormatState [String]
splitLongLines longLine indentLevel = undefined

-- | Helper to determine if a line starts a new block (e.g., ends with a colon)
isBlockStart :: String -> Bool
isBlockStart line = not (null line) && last line == ':'

-- | Helper to align if statements with else statements
isElse :: String -> [String] -> Bool
isElse line blockStack = 
    "else:" `isInfixOf` line && not (null blockStack) && ("if" `isInfixOf` head blockStack)

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
indentLine line n
    | "class " `isPrefixOf` line = trim line  -- Check if the line starts with "class"
    | otherwise = replicate (n * 4) ' ' ++ trim line  -- Add indentation otherwise

-- | Trim whitespace from the beginning of a string
trimStart :: String -> String
trimStart = dropWhile isSpace

-- | Trim whitespace from the end of a string
trimEnd :: String -> String
trimEnd = reverse . dropWhile isSpace . reverse

-- | Trim whitespace from both ends of a string
trim :: String -> String
trim = trimStart . trimEnd
