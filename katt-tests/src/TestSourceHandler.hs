module Main where

import System.Exit (exitFailure)

main :: IO ()
main = exitFailure

-- Given a set of source files, categorized into folders corresponding to all supported languages.
-- Test the following things:
-- (1) That the corresponding languages are actually identified correctly, also when intermixed.
-- (2) That all main method classes are identified correctly in the case of java and python
