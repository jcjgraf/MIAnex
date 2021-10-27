module Helper (
    Path,
    trim,
    allFilesExist,
    runProcess
) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Directory (doesFileExist)
import System.Process (ProcessHandle, waitForProcess)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO (hGetContents, Handle)

type Path = String

-- Remove leading and trailing spaces
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- Check if all files in the given list exists
-- https://stackoverflow.com/questions/3982491/determine-if-a-list-of-files-exist-in-haskell
allFilesExist :: [Path] -> IO Bool
allFilesExist files = do
    bools <- mapM doesFileExist files
    return $ foldr (&&) True bools

-- Execute given createProcess
runProcess :: IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO String
runProcess process = do
    (_, out, _, ph) <- process
    ec <- waitForProcess ph
    case out of -- TODO Simplify structure
        Just out' -> case ec of
                        ExitSuccess   -> hGetContents out' >>= return
                        ExitFailure _ -> return []
        Nothing -> return []
