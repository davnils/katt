{-# Language OverloadedStrings #-}

--------------------------------------------------------------------
-- |
-- Module : Main
--
-- Entry point of the program.
--
-- Begins by locating a global configuration file, and upon success
-- parses the given arguments and executes the corresponding submodule.
--

module Main(main) where

import qualified Configuration as C
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.Monoid ((<>))
import Init
import Network.Http.Client
import OpenSSL (withOpenSSL)
import System.Environment
import System.Exit (exitFailure)
import Upload
import Utils

-- | Main loads the global config and runs argument parsing.
main :: IO ()
main = do
  conf <- C.loadGlobalConfig
  conf' <- case conf of
    Left err -> do
      B.putStrLn $ "Kattis configuration error: " <> err
      printHelp
      exitFailure
    Right c -> return c

  withOpenSSL $ parseArgs conf'

-- | Given some configuration state, parse arguments and
--   run the appropiate submodule.
--   Output help text upon failure to parse arguments.
parseArgs :: ConfigState -> IO ()
parseArgs conf = getArgs >>= parse
  where
  parse :: [String] -> IO ()
  parse ("init" : problem : []) = withConn conf . initializeProblem True True . ProblemName $ B.pack problem
  parse ("init-session" : session : []) = withConn conf . initializeSession True $ read session
  parse ("submit" : filterList) = withConn conf $ makeSubmission filterList
  parse _ = printHelp

-- | Print help text.
printHelp :: IO ()
printHelp = putStrLn $
  "The following command are available:\n\n" <>
  "init <problem>\n" <>
  "  Creates the corresponding directory and downloads any tests.\n\n" <>
  "init-session <session>\n" <>
  "  Initialize all problems associated to the problem session (given as an integer).\n\n" <>
  "submit [+add_file] [-skip_file]\n" <>
  "  Makes a submission using the problem name associated to the current directory.\n" <>
  "  Defaults to recursively including all source and header files that can be found.\n\n" <>
  "Note that you will need a valid configuration file (placed in ~/.kattisrc), such as:\n" <>
  "https://kth.kattis.scrool.se/download/kattisrc\n"

-- | Given some action, initiate a connection and run it.
--   Connections are layered using StateT since they may be reestablished.
withConn :: ConfigState -> ConnEnv IO a -> IO a
withConn conf action = do
  B.putStrLn $ "Connecting to host: " <> host conf
  conn <- establishConnection (host conf)
  ((res, conn'), _) <- runStateT (withConf conn) conf
  closeConnection conn'
  return res

  where
  withConf = runStateT (terminateOnFailure "Failed to run command" action)
