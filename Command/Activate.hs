module Command.Activate where

import Command (ActivateOption(..))
import qualified Git as Git


runActivate :: ActivateOption -> IO ()
runActivate (ActivateImport name) = do
    exists <- Git.branchExists name
    case exists of
        True -> do
            _ <- Git.runGit ["checkout", name]
            return ()
        False -> putStrLn "Error, branch does not exists"
    return ()
