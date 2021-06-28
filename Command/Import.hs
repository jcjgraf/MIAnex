module Command.Import where

import Command (ImportOption(..))
import qualified Config as Conf
import qualified Git as Git

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
runImport (ImportImages paths opts) = do

    putStrLn $ show paths ++ show opts
    return ()

runImport _ = do
    return ()
