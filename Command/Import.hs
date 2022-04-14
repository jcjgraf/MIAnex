{-# LANGUAGE ScopedTypeVariables #-}

module Command.Import (
    runImport
) where

import Command (ImportOption(ImportImages), ImportImagesOptions(importImagesCurrentBranch, importImagesIdentifier))
import qualified Config as Conf
import Config (Schema)
import qualified Git as Git
import Helper (allFilesExist, runProcess, getImageDateTime, getFormattedImageDateTime, unifyName, Path)

import Prelude hiding (tail)
import Control.Monad (forM_)
import Data.Text (Text, empty, toUpper, toLower, pack, unpack, replace, tail)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import System.Directory (doesFileExist, createDirectoryIfMissing, copyFile)
import System.FilePath.Posix (takeExtension, takeDirectory, (</>))
import System.Process (cwd, proc, createProcess) -- TODO move fully to helper


runImport :: ImportOption -> IO ()

runImport (ImportImages [] _) = do
    putStrLn "No images provided. Import not possible"
    -- TODO show help

runImport (ImportImages paths@(firstPath:_) opts) = do

    -- TODO Check that repo is ready to be imported to
    -- TODO Make paths absolute

    -- Check if all files exists, else abort
    -- Not race-condition safe
    valid <- allFilesExist paths
    if not valid then do
        -- TODO: Throw error somehow
        putStrLn "Not all files exists. Import not possible"
        else  do -- TODO get rid of else

        -- Prepare branch
        branch <- if (importImagesCurrentBranch opts) then do
                        branch <- Git.getCurrentBranch
                        return branch
                    else do
                        mainBranch <- Conf.mainBranch
                        Git.checkoutBranch mainBranch []

                        initialCommit <- Conf.initialCommit
                        -- TODO: handle case when not date available
                        date <- getFormattedImageDateTime firstPath "%Y%m%d"
                        let branchName = case (importImagesIdentifier opts) of
                                            Just i -> date ++ "-" ++ i
                                            Nothing -> date

                        branches <- Git.getBranches
                        let uniqueBranchName = unifyName branchName branches

                        Git.checkoutBranch uniqueBranchName [initialCommit]
                        return uniqueBranchName

        putStrLn $ "Importing to branch " ++ branch

        archivePath <- Conf.archivePath

        schema <- Conf.structSchema

        -- Copy images to right place
        forM_ paths $ \path -> do
            putStrLn $ "Importing " ++ path

            relSchema <- formatSchema schema path (importImagesIdentifier opts)

            let absSchema = archivePath </> relSchema

            copyImg absSchema path

        -- Add images to git annex
        out  <- runProcess $ createProcess(proc "git-annex" ["add", "."]){ cwd = Just archivePath }
        putStrLn out
        out'  <- runProcess $ createProcess(proc "git" ["commit", "-m", "IMPORT: " ++ show (length paths) ++ " images"]){ cwd = Just archivePath }
        putStrLn out'

        -- Verify and cleanup
        return ()


formatSchema :: Schema -> Path -> Maybe String -> IO String
formatSchema rawSchema path identifier =
    -- Replaces everything except the count
    do
        -- todo: deal with images w/o metadata
        dateTime :: UTCTime <- getImageDateTime path
        let ext :: String = takeExtension path

        let replDateTime  = (\a -> pack $ formatTime defaultTimeLocale (unpack a) dateTime)
        let replIdent = case identifier of
                            Just i  -> replace (pack "*i") (pack ("-" ++ i))
                            Nothing -> replace (pack "*i") empty
        let replExtSmall | ext == "" = replace (pack "*e") empty
                         | otherwise = replace (pack "*e") (toLower (tail (pack ext)))
        let replExtLarge | ext == "" = replace (pack "*E") empty
                         | otherwise = replace (pack "*E") (toUpper (tail (pack ext)))

        let a :: Text = (replDateTime . replIdent . replExtSmall . replExtLarge) (pack rawSchema)

        return $ unpack a


formatPathSchemaCount :: Path -> Int -> Path
formatPathSchemaCount p 0  = unpack $ replace (pack "*c") (empty) (pack p)
formatPathSchemaCount p c  = unpack $ replace (pack "*c") (pack ("-" ++ (show c))) (pack p)


copyImg :: Schema -> Path -> IO ()
copyImg schema src = do
    putStrLn $ "Copying " ++ src ++ " according to " ++ schema
    aux 0
    where
        aux c = do
            let finalPath = formatPathSchemaCount schema c
            exists <- doesFileExist finalPath
            case exists of
                True  -> aux (c + 1)
                False -> do
                    putStrLn $ "Copy imgage to " ++ finalPath
                    createDirectoryIfMissing True $ takeDirectory finalPath

                    copyFile src finalPath

                    putStrLn "Copy successfuly"
