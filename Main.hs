{-# Language OverloadedStrings #-}

import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import Data.Monoid ((<>))
import Network.Http.Client
import OpenSSL (withOpenSSL)
import Upload
import Utils

main :: IO ()
main = withOpenSSL . withConnection (establishConnection host) $ runReaderT go
  where
  go = do
    session <- terminateOnFailure "Authentication failed" authenticate
    liftIO . B.putStrLn $ "[*] Temporary token: '" <> session <> "'"
    runReaderT (terminateOnFailure "Submission failed" (submitSolution (ProblemName "hello", ["example.cc"])) >>= liftIO . putStrLn . show) session
