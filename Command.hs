module Command (
    Options(optCommand),
    Command(Import, Activate, Deactivate, List, Test),
    ImportImagesOptions(importImagesIdentifier, importImagesCurrentBranch),
    ImportOption(ImportImages),
    ActivateOption(ActivateImport),
    DeactivateOption(DeactivateForce),
    mainParser
) where

import Options.Applicative (CommandFields, Mod, Parser, ParserInfo, helper, header, help, info, fullDesc, progDesc, hsubparser, command, long, switch, short, str, argument, strOption, metavar, many, optional)

import qualified Git as Git
import Helper (Path)

--- General

data Options = Options
    { optVerbose :: Bool
    , optCommand :: Command
    } deriving (Eq, Show)

data Command
    = Import ImportOption
    | Activate ActivateOption
    | Deactivate DeactivateOption
    | List
    | Test
    | NotImplemented
    deriving (Eq, Show)

--- Import
data ImportImagesOptions = ImportImagesOptions
    { importImagesIdentifier :: Maybe String
    , importImagesCombine :: Bool
    , importImagesCurrentBranch :: Bool
    } deriving (Eq, Show)

data ImportOption
    = ImportImages [Path] ImportImagesOptions
    deriving (Eq, Show)

--- Activate
data ActivateOption
    = ActivateImport Git.Branch
    deriving (Eq, Show)

-- Deactivate
data DeactivateOption
    = DeactivateForce Bool
    deriving (Eq, Show)

importCommand :: Mod CommandFields Command
importCommand =
    command "import" (info importParser (progDesc "Import to archive"))

importParser :: Parser Command
importParser =
    Import <$>
        ( ImportImages
            <$> many
                ( argument str
                    ( metavar "IMAGES"
                    <> help "Paths to images to import"
                    )
                )
            <*> ( ImportImagesOptions
                <$> optional
                    ( strOption
                        ( long "identifier"
                        <> short 'i'
                        <> metavar "IDENTIFIER"
                        <> help "Name to identify current import"
                        )
                    )
                <*> switch
                    ( long "combine"
                    <> short 'c'
                    <> help "combine stuff"
                    )
                <*> switch
                    ( long "current"
                    <> short 'C'
                    <> help "Import to current branch"
                    )
                )
    )

activateCommand :: Mod CommandFields Command
activateCommand =
    command "activate" (info activateParser (progDesc "Activate import"))

activateParser :: Parser Command
activateParser =
    Activate <$>
        ( ActivateImport
            <$> argument str
                ( metavar "IMPORT"
                <> help "Imporot to activate"
                )
        )

deactivateCommand :: Mod CommandFields Command
deactivateCommand =
    command "deactivate" (info deactivateParser (progDesc "Deactivate import"))

deactivateParser :: Parser Command
deactivateParser =
    Deactivate <$>
        ( DeactivateForce
            <$> switch
                ( long "force"
                <> short 'f'
                <> help "Force deactivate. All uncommitted changes are lost."
                )
        )

listCommand :: Mod CommandFields Command
listCommand =
    command "list" (info (pure List) (progDesc "List all imports"))

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
        <$> switch (long "verbose" <> short 'v' <> help "Enable verbose output")
        <*> hsubparser (importCommand <> activateCommand <> deactivateCommand <> listCommand <> testCommand)

mainParser :: ParserInfo Options
mainParser =
    info
        (helper <*> parseOptions)
        (fullDesc
            <> progDesc "myProg Description"
            <> header "myProg Header")
