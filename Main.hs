import Options.Applicative (execParser)

import Command (Command(Import, Activate, Deactivate, List, Test), optCommand, mainParser)
import Command.Import (runImport)
import Command.Activate (runActivate)
import Command.Deactivate (runDeactivate)
import Command.List (runList)
import Command.Test (runTest)

main :: IO ()
main = do
    options <- execParser mainParser

    case optCommand options of
        (Import opts) -> runImport opts
        (Activate opts) -> runActivate opts
        Deactivate -> runDeactivate
        List -> runList
        Test -> runTest
        _ -> putStrLn "Invalid"

    --putStrLn ("global flag: " ++ show (optVerbose options))
