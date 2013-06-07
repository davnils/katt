module Main where

import System.Exit (exitFailure)

main :: IO ()
main = exitFailure

-- Test the following things:
-- (1) downloadTestArchive returns the expected contents (as stored in /data/), for some problems
-- (2) parseProblemPage on reference page data returns the actual reference embedded data, for some problems
-- (3) initializeProblem creates the appropiate folders and creates test and config files (with garbage global config)
-- (4) initializeSession on some given session does the corresponding stuff
