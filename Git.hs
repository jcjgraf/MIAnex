module Git where

import qualified Config as Conf
import qualified Helper as H

import System.Exit
import System.Process
import System.IO (hGetContents)

type Branch = String

runGit :: [String] -> IO String
runGit args = do
    archivePath <- Conf.archivePath
    H.runProcess $ createProcess(proc "git" args){ cwd = Just archivePath , std_out = CreatePipe }

--    r <- createProcess(proc "git" args){ cwd = Just archivePath }
--    case r of
--        (_, Just stdout, _, _) -> do
--            out <- hGetContents stdout
--            return out
--        (_, Nothing, _, _) -> return []

getBranches :: IO [Branch]
getBranches = do
    out <- runGit ["--no-pager", "branch", "--list"]
    return $ map H.trim $ lines out

getCurrentBranch :: IO Branch
getCurrentBranch = do
    out <- runGit ["branch", "--show-current"]
    return $ H.trim out

branchExists :: Branch -> IO Bool
branchExists branch = do
    branches <- getBranches
    return $ branch `elem` branches

checkoutBranch :: Branch -> [String] -> IO ()
checkoutBranch branch arg = do
    exists <- branchExists branch
    -- TODO current branch has * in from

    if exists then
        do
        runGit $ ["checkout", branch] ++ arg
        putStrLn $ "Changing branch to " ++ branch
        return ()
    else
        do
        putStrLn $ "Branch " ++ branch ++ " does not exists, creating new one"
        runGit $ ["checkout", "-b", branch] ++ arg
        return ()

    return ()
