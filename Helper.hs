module Helper where

import Data.Char (isSpace)
import Data.List (dropWhile, dropWhileEnd)
import System.Process
import System.Exit
import System.IO (hGetContents, Handle)

-- Remove leading and trailing spaces
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- Execute given createProcess
runProcess :: IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO String
runProcess process = do
    (_, out, _, ph) <- process
    ec <- waitForProcess ph
    case out of -- TODO Simplify structure
        Just out -> case ec of
                        ExitSuccess   -> hGetContents out >>= return
                        ExitFailure _ -> return []
        Nothing -> return []
