{-# Language OverloadedStrings, ScopedTypeVariables #-}

module Utils where

import Control.Applicative
import Control.Error hiding (tryIO)
import Control.Exception (SomeException, try)
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import Data.List (find)
import Data.Monoid ((<>))
import Network.Http.Client
import System.Exit (exitFailure)
import System.Directory
import System.IO (stderr)
import System.IO.Streams (readExactly)

type ConnEnvInternal m = ReaderT Connection m
type ConnEnv m = EitherT ErrorDesc (ConnEnvInternal m)
type AuthEnv m = EitherT ErrorDesc (ReaderT B.ByteString (ConnEnvInternal m))
type Submission = (KattisProblem, [FilePath])
type ErrorDesc = B.ByteString
type SubmissionId = Integer

data KattisProblem
  = ProblemId Integer
  | ProblemName B.ByteString

loginSuccess :: B.ByteString
loginSuccess = "Login successful"

noauth :: (Monad m, MonadTrans t) => EitherT e m a -> EitherT e (t m) a
noauth = EitherT . lift . runEitherT

tryIO :: MonadIO m => IO a -> EitherT ErrorDesc m a
tryIO = EitherT . liftIO . liftM (fmapL (B.pack . show)) . 
  (try :: (IO a -> IO (Either SomeException a)))

terminateOnFailure :: MonadIO m => ErrorDesc -> EitherT ErrorDesc m a -> m a
terminateOnFailure msg state = do
  res <- runEitherT state
  liftIO $ case res of
    Left errorMessage -> do
      B.hPutStrLn stderr $ msg <> ", error: " <> errorMessage
      exitFailure
    Right success -> return success

makeSignedRequest :: RequestBuilder () -> AuthEnv IO Request
makeSignedRequest req = do
  key <- lift $ liftM (setHeader "Cookie") ask
  liftIO . buildRequest $ req >> key

defaultRequest :: RequestBuilder ()
defaultRequest = do
  setHeader "User-Agent" "Sofie"
  setHeader "Connection" "keep-alive"

retrievePage :: B.ByteString -> AuthEnv IO B.ByteString
retrievePage page = do
  header <- makeSignedRequest $ do
    http GET page
    defaultRequest

  conn <- lift . lift $ ask
  tryIO $ sendRequest conn header emptyBody
  tryIO $ receiveResponse conn concatHandler


-- TODO: Move these to configuration
host, user, token, loginPage :: B.ByteString
host = "https://kth.kattis.scrool.se"
user = "davnils"
token = ""
loginPage = "/login"

authenticate :: ConnEnv IO B.ByteString
authenticate = do
  header <- tryIO . buildRequest $ do
    http POST loginPage
    defaultRequest
    setContentType "application/x-www-form-urlencoded"

  let formData = [("token", token), ("user", user), ("script", "true")] 
  conn <- ask
  tryIO . sendRequest conn header $ encodedFormBody formData

  (headers, response) <- tryIO $ receiveResponse conn (\headers stream -> do
    response <- readExactly (B.length loginSuccess) stream
    return (headers, response))

  tryAssert ("Login failure. Server returned: '" <> response <> "'") (response == loginSuccess)
    
  noteT "Failed to parse login cookie" . hoistMaybe $ do
    cookies <- B.words <$> getHeader headers "Set-Cookie"
    B.takeWhile (/= ';') <$> find ("PHPSESSID" `B.isPrefixOf`) cookies

retrieveProblemId :: KattisProblem -> ConnEnv IO Integer
retrieveProblemId (ProblemId id') = return id'
retrieveProblemId (ProblemName _) = undefined

retrieveProblemName :: KattisProblem -> ConnEnv IO B.ByteString
retrieveProblemName (ProblemId _) = undefined
retrieveProblemName (ProblemName name) = return name

retrieveTestFiles :: KattisProblem -> ConnEnv IO [B.ByteString]
retrieveTestFiles = undefined

initializeProblem :: KattisProblem -> Bool -> Bool -> ConnEnv IO ()
initializeProblem problem mkDir retrieveTests = do
  tryIO $ createDirectoryIfMissing False "<problem>"
  {- setCurrentDirectory ""
  createDirectoryIfMissing ".sofie" -}
  _ <- undefined problem
  _ <- undefined mkDir
  _ <- undefined retrieveTests
  return ()
