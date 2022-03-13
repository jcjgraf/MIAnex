module Git(
    Branch,
    runGit,
    getBranches,
    getCurrentBranch,
    branchExists,
    checkoutBranch,
    existUncommittedChanges
) where

import qualified Config as Conf
import qualified Helper as H

import System.Exit ()
import System.Process (createProcess, cwd, std_out, proc, StdStream(CreatePipe))
--import System.IO (hGetContents)

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
    return $ map gitTrim $ lines out

getCurrentBranch :: IO Branch
getCurrentBranch = do
    out <- runGit ["branch", "--show-current"]
    return $ gitTrim out

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
        _ <- runGit $ ["checkout", branch] ++ arg
        putStrLn $ "Changing branch to " ++ branch
        return ()
    else
        do
        putStrLn $ "Branch " ++ branch ++ " does not exists, creating new one"
        _ <- runGit $ ["checkout", "-b", branch] ++ arg
        return ()

    return ()

existUncommittedChanges :: IO Bool
existUncommittedChanges = do
    out <- runGit $ ["status", "-s"]
    if out == "" then
        return False
    else
        return True

dropStar :: Branch -> Branch
dropStar = dropWhile (== '*')

gitTrim :: Branch -> Branch
gitTrim = H.trim . dropStar . H.trim
