module Main where

import           Data.List
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.IO
import qualified Commands.Greet as Greet
import qualified Commands.GroupFiles as GroupFiles
import qualified Commands.Insult as Insult


data Options = Options
    { optCommand :: Command, optQuiet :: Bool }


data Command
    = Greet Greet.Options
    | Insult Insult.Options
    | GroupFiles GroupFiles.Options


main :: IO ()
main =
    execParser opts >>= parseOptions
  where
    opts = info (helper <*> versionOption <*> optionsParser)
        ( fullDesc
        <> header "name – a test for optparse-applicative"
        <> progDesc "CLI playground to try out writing a CLI with Haskell."
        )


versionOption :: Parser (a -> a)
versionOption = infoOption "0.0" (long "version" <> help "Show version")


optionsParser :: Parser Options
optionsParser = Options
    <$> commandParser
    <*> quietParser


quietParser :: Parser Bool
quietParser = switch ( long "quiet" <> short 'q' <> help "Suppress output" )


commandParser :: Parser Command
commandParser = hsubparser
    ( command
        "greet"
        (info greetParser ( progDesc "Greet a person!" ))
    <> command
        "insult"
        (info insultParser (progDesc "Insult a person!" ))
    <> command
        "groupFiles"
        (info groupFilesParser (progDesc "Group files into folders"))
    )


greetParser :: Parser Command
greetParser = Greet
    <$> ( Greet.Options
        <$> strOption
            ( long "name"
            <> metavar "TARGET"
            <> help "Target for the greeting"
            )
        <*> option auto
            ( long "enthusiasm"
            <> help "How enthusiastically to greet"
            <> showDefault
            <> value 1
            <> metavar "INT"
            )
    )


insultParser :: Parser Command
insultParser = Insult
    <$> ( Insult.Options
        <$> strOption
            ( long "name" <> metavar "TARGET" <> help "Who to insult" )
        <*> strOption
            ( long "insult" <> metavar "INSULT" <> help "What to insult" )
    )


groupFilesParser :: Parser Command
groupFilesParser = GroupFiles
    <$> ( GroupFiles.Options
        <$> strArgument (metavar "FOLDER" <> help "The folder to look for files in")
    )


parseOptions :: Options -> IO ()
parseOptions Options { optQuiet = True, optCommand = cmd } = putStrLn "Quiet."
parseOptions Options { optQuiet = False, optCommand = cmd} = case cmd of
    Greet options -> Greet.run options
    Insult options -> Insult.run options
    GroupFiles options -> GroupFiles.run options
