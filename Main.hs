{-# Language OverloadedStrings #-}

import qualified Configuration as C
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.Monoid ((<>))
import Init
import Network.Http.Client
import OpenSSL
import System.Environment
import System.Exit (exitFailure)
import Upload
import Utils

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

parseArgs :: ConfigState -> IO ()
parseArgs conf = getArgs >>= parse

  where
  parse :: [String] -> IO ()
  parse ("init" : problem : []) = withConn conf . initializeProblem True True . ProblemName $ B.pack problem
  parse ("submit" : _) = withConn conf $ makeSubmission
  parse _ = printHelp

printHelp :: IO ()
printHelp = putStrLn $
  "The following command are available:\n\n" <>
  "init <problem>\n" <>
  "  Creates the corresponding directory and downloads any tests.\n\n" <>
  "submit \n" <>
  "  Makes a submission using the problem name associated to the current directory.\n" <>
  "  Defaults to recursively including all source and header files that can be found.\n\n" <>
  "Note that you will need a valid configuration file (placed in ~/.kattisrc), such as:\n" <>
  "https://kth.kattis.scrool.se/download/kattisrc\n"

withConn :: ConfigState -> ConnEnv IO a -> IO a
withConn conf action = do
  conn <- liftIO $ establishConnection (host conf)
  (res, _) <- runStateT (withConf conn) conf
  liftIO $ closeConnection conn
  return res

  where
  withConf = runReaderT (terminateOnFailure "Failed to run command" action)
