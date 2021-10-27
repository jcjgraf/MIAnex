module Command.List where

import qualified Git as Git


runList :: IO ()
runList = do
    branches <- Git.getBranches
    putStrLn $ unlines branches
    return ()
