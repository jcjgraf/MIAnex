module Helper where

import Data.Char (isSpace)
import Data.List (dropWhile, dropWhileEnd)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
