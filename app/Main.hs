module Main where

import Options.Applicative

type ItemIndex = Int
type ItemDescription = Maybe String

data Options = Options FilePath ItemIndex ItemDescription deriving Show

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto $ metavar "ITEMINDEX" <> help "index of item"


updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser =
  Just <$> itemDescriptionValueParser
  <|> flag' Nothing (long "clear-desc")


itemDescriptionValueParser :: Parser String
itemDescriptionValueParser =
  strOption (long "desc" <> short 'd' <> metavar "DESCRIPTION" <> help "description")

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"


optionsParser :: Parser Options
optionsParser = Options
  <$> dataPathParser
  <*> itemIndexParser
  <*> updateItemDescriptionParser


dataPathParser :: Parser FilePath
dataPathParser = strOption $
  value defaultDataPath
  <> long "data-path"
  <> short 'p'
  <> metavar "DATAPATH"
  <> help ("path to data file (default " ++ defaultDataPath ++ ")")


main :: IO ()
main = do
  options <- execParser (info (optionsParser) (progDesc "Todo list manager"))
  putStrLn $ "options" ++ show options
