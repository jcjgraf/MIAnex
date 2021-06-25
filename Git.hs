module Git where

import Config
import qualified Helper as H

import System.Exit
import System.Process
import System.IO ( hGetContents )

type Branch = String

runGit :: [String] -> IO String
runGit args = do
    (_, Just out, _, ph) <- createProcess(proc "git" args){ cwd = Just archivePath , std_out = CreatePipe }
    ec <- waitForProcess ph
    case ec of
        ExitSuccess   -> hGetContents out >>= return
        ExitFailure _ -> return []

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

branchExists :: Branch -> IO Bool
branchExists branch = do
    branches <- getBranches
    return $ branch `elem` branches
