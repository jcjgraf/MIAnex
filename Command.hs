module Command where

import Data.Semigroup ((<>))
import Options.Applicative

--- General

data Options = Options
    { optGlobalFlag :: Bool
    , optCommand :: Command
    } deriving (Eq, Show)

data Command
    = Import ImportOptions
    | Test
    | NotImplemented
    deriving (Eq, Show)

--- Import
data ImportOptions
    = New String
    | Activate String
    | List Bool
    deriving (Eq, Show)

importCommand :: Mod CommandFields Command
importCommand =
    command "import" (info importParser (progDesc "Import to archive"))

importParser :: Parser Command
importParser =
    Import <$>
        ( New 
            <$> strOption
                ( long "new"
                <> short 'n'
                <> metavar "NAME"
                <> help "Name of the new import"
                )
        <|> Activate
            <$> strOption
                ( long "activate"
                <> short 'a'
                <> metavar "IMPORT"
                <> help "Name of the import to activate"
                )
        <|> List
            <$> switch
                ( long "list"
                <> short 'l'
                <> help "List all imports"
                )
    )

--- Test
--data TestOptions = TestOptions
 
testCommand :: Mod CommandFields Command
testCommand =
    command "test" (info (pure NotImplemented) (progDesc "Test something"))

--testParser :: Parser TestOptions
--testParser =
--    TestOptions

--- Parser
parseOptions :: Parser Options
parseOptions =
    Options
        <$> switch (long "global-flag" <> help "Set a global flag")
        <*> hsubparser (importCommand <> testCommand)

mainParser :: ParserInfo Options
mainParser =
    info
        (helper <*> parseOptions)
        (fullDesc
            <> progDesc "myProg Description"
            <> header "myProg Header")
