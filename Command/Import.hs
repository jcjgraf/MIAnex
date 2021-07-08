module Command.Import where

import Command (ImportOption(..), ImportImagesOptions(..))
import qualified Config as Conf
import qualified Git as Git
import Helper (allFilesExist, getDate, getTime, runProcess)

import Data.Text (pack, unpack)
import Text.StringRandom (stringRandomIO)
import System.Process hiding (runProcess) -- TODO move fully to helper
import Control.Monad (forM_)


runImport :: ImportOption -> IO ()

-- activate
runImport (ImportActivate name) = do
    exists <- Git.branchExists name
    case exists of
        True -> do
            _ <- Git.runGit ["checkout", name]
            return ()
        False -> putStrLn "Error, branch does not exists"
    return ()

-- deactivate
runImport (ImportDeactivate True) = do
    _ <- Git.runGit ["checkout", Conf.mainBranch]
    return ()

-- list
runImport (ImportList True) = do
    branches <- Git.getBranches
    putStrLn $ unlines branches
    return ()

-- import
runImport (ImportImages paths@(x:xs) opts) = do

    putStrLn $ show paths ++ show opts ++ importImagesIdentifier opts

    -- Check if all files exists, else abort
    -- Not race-condition safe
    valid <- allFilesExist paths
    if not valid then do
        -- TODO: Throw error somehow
        putStrLn "Not all files exists. Import not possible"
        else  do -- TODO get rid of else

        -- Prepare branch
        Git.checkoutBranch Conf.mainBranch []

        identifier <- stringRandomIO (pack "[0-9a-zA-Z]{10}")
        Git.checkoutBranch ("TEST-" ++ (unpack identifier) ++ "-" ++ importImagesIdentifier opts) [Conf.initialCommit]

        -- Copy images to right place
        forM_ paths $ \path -> do
            putStrLn $ "Importing " ++ path

            out <- runProcess $ createProcess(proc "exiftool" ["-o", ".", "-FileName<CreateDate", "-d", "%Y/%y%m%d-" ++ importImagesIdentifier opts ++ "/%y%m%d_%H%M%S%%-c.%%le", "-r", path]){ cwd = Just Conf.archivePath }
            putStrLn out
            return ()
        putStrLn "asdfadfs"
        -- Add images to git annex
        out  <- runProcess $ createProcess(proc "git-annex" ["add", "."]){ cwd = Just Conf.archivePath }
        putStrLn out
        out  <- runProcess $ createProcess(proc "git" ["commit", "-m", "INITIAL IMPORT: " ++ show (length paths) ++ " images"]){ cwd = Just Conf.archivePath }
        putStrLn out

        -- Verify and cleanup
        return ()

runImport _ = do
    return ()
