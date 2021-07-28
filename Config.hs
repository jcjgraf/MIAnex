module Config where

import Data.Ini
import Data.Text

getOption :: String -> String -> IO String
getOption group option = do
    config <- readIniFile "/home/jeanclaude/.config/mia/config.ini"
    -- TODO xdg base dir
    -- TODO case when file missing
    case config of
        Right ini -> do
            let p = lookupValue (pack group) (pack option) ini
            return $ case p of
                        Left s -> s
                        Right t -> unpack t
        Left s -> do
            putStrLn s
            return ""

archivePath :: IO String
archivePath = getOption "GENERAL" "archivePath"

mainBranch :: IO String
mainBranch = getOption "GENERAL" "mainBranch"

importBranch :: IO String
importBranch = getOption "GENERAL" "importBranch"

initialCommit :: IO String
initialCommit = getOption "GENERAL" "initialCommit"
