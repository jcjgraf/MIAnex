module Command.Deactivate (
runDeactivate
) where

import Command (DeactivateOption(DeactivateForce))
import qualified Config as Conf
import qualified Git as Git


runDeactivate :: DeactivateOption -> IO ()
runDeactivate (DeactivateForce force) = do
    isDirty <- Git.existUncommittedChanges
    if (isDirty && (not force)) then do
        putStrLn "Deactivation not possible as there are uncommitted changes. Commit or discard them, or use the force flag to discard them."
        return ()
    else do
        mainBranch <- Conf.mainBranch
        _ <- Git.runGit ["checkout", mainBranch]
        return ()
