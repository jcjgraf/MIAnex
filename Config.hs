module Config (
    getOption,
    archivePath,
    mainBranch,
    importBranch,
    initialCommit,
    schema,
    defaultSchema,
    Schema
) where

import Data.Ini (readIniFile, lookupValue)
import Data.Text (pack, unpack)
import System.Directory (getXdgDirectory, XdgDirectory( XdgConfig ), doesFileExist)

type Schema = String

getOption :: String -> String -> IO String
getOption group option = do
    confFile <- getXdgDirectory XdgConfig "mia/config.ini"
    exists <- doesFileExist confFile
    if exists then do
        config <- readIniFile confFile
        case config of
            Right ini -> do
                let p = lookupValue (pack group) (pack option) ini
                return $ case p of
                            Left s -> s
                            Right t -> unpack t
            Left s -> do
                putStrLn s
                return ""
        else do
            putStrLn $ "Config " ++ (show confFile) ++ " does not exist"
            return ""

archivePath :: IO String
archivePath = getOption "GENERAL" "archivePath"

mainBranch :: IO String
mainBranch = getOption "GENERAL" "mainBranch"

importBranch :: IO String
importBranch = getOption "GENERAL" "importBranch"

initialCommit :: IO String
initialCommit = getOption "GENERAL" "initialCommit"

schema :: String -> IO Schema
schema = getOption "SCHEMA"

defaultSchema :: IO Schema
defaultSchema = schema "default"
