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
-- import Upload
import Utils

main :: IO ()
main = do
  conf <- C.loadGlobalConfig
  conf' <- case conf of
    Left err -> B.putStrLn ("Kattis configuration error: " <> err) >> exitFailure
    Right c -> return c

  (problem : []) <- getArgs
  runProgram conf' (B.pack problem)

runProgram conf problem = withOpenSSL $ do
  conn <- establishConnection (host conf)
  B.putStrLn $ "Retrieving problem: " <> problem
  evalStateT (initialize conn) conf
  closeConnection conn

  where
  initialize = runReaderT (terminateOnFailure "Failed to initialize problem" go)
  go = do
    -- testData <- downloadTestArchive "/download/sampledata?id=maxloot"
    -- liftIO $ print testData
    initializeProblem (ProblemName problem) True True
    {- session <- terminateOnFailure "Authentication failed" authenticate
    liftIO . B.putStrLn $ "[*] Temporary token: '" <> session <> "'"
    runReaderT (terminateOnFailure "Submission failed" (submitSolution (ProblemName "hello", ["example.cc"])) >>= liftIO . putStrLn . show) session -}

