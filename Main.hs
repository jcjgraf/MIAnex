import Data.Semigroup ((<>))
import Options.Applicative

import Command
import Command.Import (runImport)
import Command.Test

main :: IO ()
main = do
    options <- execParser mainParser

    case optCommand options of
        (Import opts) -> runImport opts
        Test -> putStrLn "Test Stuff!"

    putStrLn ("global flag: " ++ show (optGlobalFlag options))
