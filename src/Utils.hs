{-# Language OverloadedStrings, ScopedTypeVariables,
    NoMonomorphismRestriction #-}

module Utils where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay) -- TODO: REMOVE !!!
import Control.Error hiding (tryIO)
import qualified Control.Exception as E
import Control.Monad.Reader
import qualified Control.Monad.State as S
import qualified Data.ByteString.Char8 as B
import Data.Monoid ((<>))
import Network.Http.Client
import System.Exit (exitFailure)
import System.IO (stderr)
import System.IO.Streams (readExactly)

type ConfigEnvInternal m = S.StateT ConfigState m
type ConfigEnv m = EitherT ErrorDesc (ConfigEnvInternal m)
type ConnEnvInternal m = S.StateT Connection (ConfigEnvInternal m)
type ConnEnv m = EitherT ErrorDesc (ConnEnvInternal m)
type AuthEnv m = EitherT ErrorDesc (ReaderT B.ByteString (ConnEnvInternal m))

type Submission = (KattisProblem, [FilePath])
type ErrorDesc = B.ByteString
type SubmissionId = Integer
type ProblemSession = Integer
type ProjectState = (KattisProblem)

data ConfigState =
  ConfigState {
    user :: B.ByteString,
    apiKey :: B.ByteString,
    host :: B.ByteString,
    loginPage :: B.ByteString,
    submitPage :: B.ByteString,
    project :: Maybe ProjectState
  }
  deriving Show

data KattisProblem
  = ProblemId Integer
  | ProblemName B.ByteString
  deriving (Eq, Show)

data KattisLanguage
  = LangCplusplus
  | LangJava
  | LangC
  deriving (Eq, Show)

loginSuccess :: B.ByteString
loginSuccess = "Login successful"

inputTestExtension :: FilePath
inputTestExtension = ".in"

outputTestExtension :: FilePath
outputTestExtension = ".ans"

programName :: B.ByteString
programName = "sofie"

configDir :: B.ByteString
configDir = "." <> programName

testFolder :: FilePath
testFolder = "tests"

problemAddress :: B.ByteString
problemAddress = "/problems/"

unWrapTrans :: (Monad m, MonadTrans t) => EitherT e m a -> EitherT e (t m) a
unWrapTrans = EitherT . lift . runEitherT

tryIO :: MonadIO m => IO a -> EitherT ErrorDesc m a
tryIO = EitherT . liftIO . liftM (fmapL (B.pack . show)) . 
  (E.try :: (IO a -> IO (Either E.SomeException a)))

tryIOMsg :: MonadIO m => B.ByteString -> IO a -> EitherT ErrorDesc m a
tryIOMsg msg = EitherT . liftIO . liftM (fmapL $ const msg) . 
  (E.try :: (IO a -> IO (Either E.SomeException a)))

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

reestablishConnection :: ConnEnv IO ()
reestablishConnection = do
  conn <- lift S.get
  tryIOMsg "Failed to close connection" $ closeConnection conn
  host' <- host <$> lift (lift S.get)
  tryIO $ threadDelay 100000
  ctx <- tryIO $ baselineContextSSL
  conn' <- tryIOMsg "Failed to reestablish connection" $ openConnectionSSL ctx host' 443
  lift $ S.put conn'

retrievePublicPage :: B.ByteString -> ConnEnv IO B.ByteString
retrievePublicPage page = do
  header <- tryIO . buildRequest $ http GET page >> defaultRequest
  makeRequest header

retrievePrivatePage :: B.ByteString -> AuthEnv IO B.ByteString
retrievePrivatePage page = do
  header <- makeSignedRequest $ do
    http GET page
    defaultRequest
  unWrapTrans $ makeRequest header

makeRequest :: Request -> ConnEnv IO B.ByteString
makeRequest header = do
  conn <- lift S.get
  tryIO $ sendRequest conn header emptyBody
  tryIO $ receiveResponse conn concatHandler

extractSessionHeader :: B.ByteString -> Maybe B.ByteString
extractSessionHeader headerStr 
  | B.null match = Nothing
  | otherwise =
    case extractSessionHeader (B.tail match) of
      Just match' -> Just match'
      Nothing -> Just $ B.takeWhile (/= ';') match
  where
  (_, match) = B.breakSubstring "PHPSESSID" headerStr

authenticate :: ConnEnv IO B.ByteString
authenticate = do
  conf <- lift $ lift S.get

  header <- tryIO . buildRequest $ do
    http POST ("/" <> loginPage conf)
    defaultRequest
    setContentType "application/x-www-form-urlencoded"

  let formData = [("token", apiKey conf), ("user", user conf), ("script", "true")] 
  conn <- S.get
  tryIO . sendRequest conn header $ encodedFormBody formData

  (headers, response) <- tryIO $ receiveResponse conn (\headers stream -> do
    response <- readExactly (B.length loginSuccess) stream
    return (headers, response))

  let Just h = getHeader headers "Set-Cookie"
  liftIO $ putStrLn $ show h

  tryAssert ("Login failure. Server returned: '" <> response <> "'")
    (response == loginSuccess)
    
  noteT "Failed to parse login cookie" . hoistMaybe $ 
    getHeader headers "Set-Cookie" >>= extractSessionHeader

retrieveProblemId :: KattisProblem -> ConnEnv IO Integer
retrieveProblemId (ProblemId id') = return id'
retrieveProblemId (ProblemName _) = undefined

retrieveProblemName :: KattisProblem -> ConnEnv IO B.ByteString
retrieveProblemName (ProblemId _) = undefined
retrieveProblemName (ProblemName name) = return name
