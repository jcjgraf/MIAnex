{-# LANGUAGE ScopedTypeVariables #-}

module Command.Import (
    runImport
) where

import Command (ImportOption(ImportImages), ImportImagesOptions(importImagesCurrentBranch, importImagesIdentifier))
import qualified Config as Conf
import qualified Git as Git
import Helper (allFilesExist, runProcess, getImageDate, unifyName)

import Data.Text (pack, unpack, replace)
import System.Process (cwd, proc, createProcess) -- TODO move fully to helper
import Control.Monad (forM_)


runImport :: ImportOption -> IO ()

runImport (ImportImages [] _) = do
    putStrLn "No images provided. Import not possible"
    -- TODO show help

runImport (ImportImages paths@(firstPath:_) opts) = do
    
    -- TODO Check that repo is ready to be imported to

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
                        date <- getImageDate firstPath
                        let branchName = if null (importImagesIdentifier opts) then
                                            date
                                        else
                                            date ++ "-" ++ (importImagesIdentifier opts)

                        branches <- Git.getBranches
                        let uniqueBranchName = unifyName branchName branches

                        Git.checkoutBranch uniqueBranchName [initialCommit]
                        return uniqueBranchName

        putStrLn $ "Importing to branch " ++ branch

        archivePath <- Conf.archivePath

        -- Determine naming structure
        structure <- if null (importImagesIdentifier opts) then do
                        structure <- Conf.structN
                        return structure
                    else do
                        s <- Conf.structNI
                        let structure = unpack $ replace (pack "IDENTIFIER") (pack (importImagesIdentifier opts)) (pack s)
                        return structure

        -- Copy images to right place
        forM_ paths $ \path -> do
            putStrLn $ "Importing " ++ path

            out <- runProcess $ createProcess(proc "exiftool" ["-o", ".", "-FileName<CreateDate", "-d", structure, "-r", path]){ cwd = Just archivePath }
            putStrLn out
            return ()

        -- Add images to git annex
        out  <- runProcess $ createProcess(proc "git-annex" ["add", "."]){ cwd = Just archivePath }
        putStrLn out
        out'  <- runProcess $ createProcess(proc "git" ["commit", "-m", "IMPORT: " ++ show (length paths) ++ " images"]){ cwd = Just archivePath }
        putStrLn out'

        -- Verify and cleanup
        return ()
