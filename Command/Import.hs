module Command.Import where

import Command (ImportOptions(..))
import qualified Config as Conf
import qualified Git as Git

runImport :: ImportOptions -> IO ()
runImport (ImportNew name) = do
    return ()

runImport (ImportActivate name) = do
    exists <- Git.branchExists name
    case exists of
        True -> do
            _ <- Git.runGit ["checkout", name]
            return ()
        False -> putStrLn "Error, branch does not exists"
    return ()

runImport (ImportDeactivate True) = do
    _ <- Git.runGit ["checkout", Conf.mainBranch]
    return ()

runImport (ImportList True) = do
    branches <- Git.getBranches
    putStrLn $ unlines branches
    return ()

runImport _ = do
    return ()
