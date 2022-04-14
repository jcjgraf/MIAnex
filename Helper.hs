{-# LANGUAGE ScopedTypeVariables #-}

module Helper (
    Path,
    trim,
    allFilesExist,
    runProcess,
    getImageDateTime,
    getFormattedImageDateTime,
    unifyName
) where

import qualified Config as Conf

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Time (UTCTime, parseTimeOrError, defaultTimeLocale, formatTime)
import System.Directory (doesFileExist)
import System.Process (ProcessHandle, waitForProcess, cwd, proc, std_out, createProcess, StdStream(CreatePipe))
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


getImageDateTime :: Path -> IO UTCTime
getImageDateTime path = do
    archivePath <- Conf.archivePath
    -- TODO Error Handling
    out <- runProcess $ createProcess(proc "exiftool" ["-short3", "-dateFormat", "%Y:%m:%d %H:%M:%S", "-EXIF:CreateDate", path]){ cwd = Just archivePath, std_out = CreatePipe }

    let dateTime :: UTCTime = parseTimeOrError True defaultTimeLocale "%0Y:%m:%d %H:%M:%S" (trim out)
    return dateTime


getFormattedImageDateTime :: Path -> String -> IO String
getFormattedImageDateTime path format = do
    dateTime :: UTCTime <- getImageDateTime path
    let dateTimeString :: String = formatTime defaultTimeLocale format dateTime
    return dateTimeString


showZeroPad :: Int -> String
showZeroPad n
    | length (show n) == 1 = "0" ++ (show n)
    | otherwise            = show n


unifyName :: String -> [String] -> String
unifyName name nameSet = aux name 1
    where
        aux :: String -> Int -> String
        aux nameTest count
            | (not (nameTest `elem` nameSet)) = nameTest
            | otherwise = (aux (name ++ "-" ++ (showZeroPad count)) (count + 1))
