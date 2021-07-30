module Config where

import Data.Ini
import Data.Text
import System.Directory

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

structN :: IO String
structN = getOption "STRUCTURE" "normal"

structNI :: IO String
structNI = getOption "STRUCTURE" "normalIdentifier"
