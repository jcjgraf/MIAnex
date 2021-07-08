module Command where

import Data.Semigroup ((<>))
import Options.Applicative

import qualified Git as Git
import Helper

--- General

data Options = Options
    { optVerbose :: Bool
    , optCommand :: Command
    } deriving (Eq, Show)

data Command
    = Import ImportOption
    | Test
    | NotImplemented
    deriving (Eq, Show)

--- Import
data ImportImagesOptions = ImportImagesOptions
    { importImagesIdentifier :: String
    , importImagesCombine :: Bool
    } deriving (Eq, Show)

data ImportOption
    = ImportImages [Path] ImportImagesOptions
    | ImportActivate Git.Branch
    | ImportDeactivate Bool
    | ImportList Bool
    deriving (Eq, Show)

importCommand :: Mod CommandFields Command
importCommand =
    command "import" (info importParser (progDesc "Import to archive"))

importParser :: Parser Command
importParser =
    Import <$>
        ( ImportImages
            <$> many ( argument str
                ( metavar "IMAGES"
                <> help "Paths to images to import"
                ))
            <*> ( ImportImagesOptions
                <$> strOption
                    ( long "identifer"
                    <> short 'i'
                    <> metavar "IDENTIFIER"
                    <> help "Name to identify current import"
                    <> value ""
                    )
                <*> switch
                    ( long "combine"
                    <> short 'c'
                    <> help "combine stuff"
                    )
                )
        <|> ImportActivate
            <$> strOption
                ( long "activate"
                <> short 'a'
                <> metavar "IMPORT"
                <> help "Name of the import to activate"
                )
        <|> ImportDeactivate
            <$> switch
                ( long "deactivate"
                <> short 'd'
                <> help "Deactivate the import"
                )
        <|> ImportList
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
        <$> switch (long "verbose" <> short 'v' <> help "Enable verbose output")
        <*> hsubparser (importCommand <> testCommand)

mainParser :: ParserInfo Options
mainParser =
    info
        (helper <*> parseOptions)
        (fullDesc
            <> progDesc "myProg Description"
            <> header "myProg Header")
