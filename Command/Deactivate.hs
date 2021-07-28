module Command.Deactivate where

import qualified Config as Conf
import qualified Git as Git


runDeactivate :: IO ()
runDeactivate = do
    mainBranch <- Conf.mainBranch
    _ <- Git.runGit ["checkout", mainBranch]
    return ()
