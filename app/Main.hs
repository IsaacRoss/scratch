module Main where

import           Options.Applicative hiding (infoParser)

type ItemIndex = Int

type ItemDescription = Maybe String

data Options =
  Options FilePath
          Command
  deriving (Show)

data Command
  = Info
  | Init
  | List
  | Add
  | View
  | Update
  | Remove
  deriving (Show)

infoParser :: Parser Command
infoParser = pure Info

initParser :: Parser Command
initParser = pure Init

listParser :: Parser Command
listParser = pure List

addParser :: Parser Command
addParser = pure Add

viewParser :: Parser Command
viewParser = pure View

updateParser :: Parser Command
updateParser = pure Update

removeParser :: Parser Command
removeParser = pure Remove

commandParser :: Parser Command
commandParser =
  subparser $
  mconcat
    [ command "info" (info infoParser (progDesc "Show Info"))
    , command "init" (info initParser (progDesc "Initialize Items"))
    , command "list" (info listParser (progDesc "List all Items"))
    , command "add" (info addParser (progDesc "Add Item"))
    , command "view" (info viewParser (progDesc "View Item"))
    , command "update" (info updateParser (progDesc "Update Item"))
    , command "remove" (info removeParser (progDesc "Remove Item"))
    ]

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto $ metavar "ITEMINDEX" <> help "index of item"

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser =
  Just <$> itemDescriptionValueParser <|> flag' Nothing (long "clear-desc")

itemDescriptionValueParser :: Parser String
itemDescriptionValueParser =
  strOption
    (long "desc" <> short 'd' <> metavar "DESCRIPTION" <> help "description")

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"

optionsParser :: Parser Options
optionsParser = Options <$> dataPathParser <*> commandParser

dataPathParser :: Parser FilePath
dataPathParser =
  strOption $
  value defaultDataPath <> long "data-path" <> short 'p' <> metavar "DATAPATH" <>
  help ("path to data file (default " ++ defaultDataPath ++ ")")

main :: IO ()
main = do
  options <- execParser (info (optionsParser) (progDesc "Todo list manager"))
  putStrLn $ "options" ++ show options
