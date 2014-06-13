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

import qualified Utils.Katt.Configuration as C
import qualified Control.Monad.State as S
import qualified Data.ByteString.Char8 as B
import           Data.Monoid ((<>))
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           Utils.Katt.Init
import           Utils.Katt.Upload
import           Utils.Katt.Utils

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

  parseArgs conf'

-- | Given some configuration state, parse arguments and
--   run the appropiate submodule.
--   Output help text upon failure to parse arguments.
parseArgs :: ConfigState -> IO ()
parseArgs conf = getArgs >>= parse
  where
  parse :: [String] -> IO ()
  parse ("init" : problem : []) = withConf conf . initializeProblem True True . ProblemName $ B.pack problem
  parse ("init-session" : session : []) = withConf conf . initializeSession True $ read session
  parse ("submit" : filterList) = withConf conf $ makeSubmission filterList
  parse _ = printHelp

-- | Print help text.
printHelp :: IO ()
printHelp = putStrLn $
  "The following commands are available:\n\n" <>
  "init <problem>\n" <>
  "  Create the corresponding directory and download any available tests.\n\n" <>
  "init-session <session>\n" <>
  "  Initialize all problems associated to the problem session, given as an integer id.\n\n" <>
  "submit [+add_file] [-skip_file]\n" <>
  "  Make a submission using the problem name read from a local config.\n" <>
  "  Defaults to recursively including all source and header files that can be found.\n" <>
  "  Use the optional filter arguments to include or exclude files.\n\n" <>
  "Note that you will need a valid configuration file (placed in ~/.kattisrc), such as:\n" <>
  "https://www.kattis.com/download/kattisrc\n"

-- | Run an action with the supplied configuration.
withConf :: ConfigState -> ConfigEnv IO a -> IO a
withConf conf action = do
  (res, _) <- S.runStateT (terminateOnFailure "Failed to run command" action) conf
  return res
