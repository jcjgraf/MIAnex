import Options.Applicative

import Command
import Command.Import (runImport)
import Command.Activate (runActivate)
import Command.Deactivate (runDeactivate)
import Command.Test

main :: IO ()
main = do
    options <- execParser mainParser

    case optCommand options of
        (Import opts) -> runImport opts
        (Activate opts) -> runActivate opts
        Deactivate -> runDeactivate
        Test -> putStrLn "Test Stuff!"
        _ -> putStrLn "Invalid"

    --putStrLn ("global flag: " ++ show (optVerbose options))
