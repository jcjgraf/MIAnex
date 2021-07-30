module Command.Import where

import Command (ImportOption(..), ImportImagesOptions(..))
import qualified Config as Conf
import qualified Git as Git
import Helper (allFilesExist, runProcess)

import Data.Text (pack, unpack)
import Text.StringRandom (stringRandomIO)
import System.Process hiding (runProcess) -- TODO move fully to helper
import Control.Monad (forM_)


runImport :: ImportOption -> IO ()

runImport (ImportImages [] opts) = do
    putStrLn "No images provided. Import not possible"
    -- TODO show help

runImport (ImportImages paths@(x:xs) opts) = do

    -- Check if all files exists, else abort
    -- Not race-condition safe
    valid <- allFilesExist paths
    if not valid then do
        -- TODO: Throw error somehow
        putStrLn "Not all files exists. Import not possible"
        else  do -- TODO get rid of else

        -- Prepare branch
        mainBranch <- Conf.mainBranch
        Git.checkoutBranch mainBranch []

        initialCommit <- Conf.initialCommit
        -- TODO: Make sure branch does not already exist
        identifier <- stringRandomIO (pack "[0-9a-zA-Z]{10}")
        Git.checkoutBranch ((unpack identifier) ++ "-" ++ importImagesIdentifier opts) [initialCommit]

        archivePath <- Conf.archivePath

        -- Copy images to right place
        forM_ paths $ \path -> do
            putStrLn $ "Importing " ++ path

            out <- runProcess $ createProcess(proc "exiftool" ["-o", ".", "-FileName<CreateDate", "-d", "%Y/%y%m%d-" ++ importImagesIdentifier opts ++ "/%y%m%d_%H%M%S%%-c.%%le", "-r", path]){ cwd = Just archivePath }
            putStrLn out
            return ()

        -- Add images to git annex
        out  <- runProcess $ createProcess(proc "git-annex" ["add", "."]){ cwd = Just archivePath }
        putStrLn out
        out  <- runProcess $ createProcess(proc "git" ["commit", "-m", "IMPORT: " ++ show (length paths) ++ " images"]){ cwd = Just archivePath }
        putStrLn out

        -- Verify and cleanup
        return ()

-- List
runImport (ImportList True) = do
    branches <- Git.getBranches
    putStrLn $ unlines branches
    return ()
