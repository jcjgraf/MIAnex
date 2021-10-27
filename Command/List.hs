module Command.List (
    runList
) where

import Data.Text (pack, isInfixOf)
import qualified Config as Conf

import qualified Git as Git


runList :: IO ()
runList = do
    branches <- Git.getBranches
    mainBranch <- Conf.mainBranch

    -- TODO: make filter more clean and general
    let filterStrings = map pack ["synced/", "git-annex", mainBranch]
    let branchesFiltered1 = filter (\e -> not $ isInfixOf (filterStrings !! 0) (pack e)) branches
    let branchesFiltered2 = filter (\e -> not $ isInfixOf (filterStrings !! 1) (pack e)) branchesFiltered1
    let branchesFiltered3 = filter (\e -> not $ isInfixOf (filterStrings !! 2) (pack e)) branchesFiltered2
    putStrLn $ unlines branchesFiltered3
    return ()
