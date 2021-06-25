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
        _ -> putStrLn "Invalid"

    putStrLn ("global flag: " ++ show (optVerbose options))
