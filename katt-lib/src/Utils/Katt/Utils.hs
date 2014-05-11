{-# Language OverloadedStrings #-}

--------------------------------------------------------------------
-- |
-- Module : Utils.Katt.Utils
--
-- Contains shared type declarations and various utility functions.
--

module
Utils.Katt.Utils
where

import Control.Applicative ((<$>))
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

-- | Configuration layer consisting of configuration state.
type ConfigEnvInternal m = S.StateT ConfigState m
-- | Configuration layer wrapped with error handling.
type ConfigEnv m = EitherT ErrorDesc (ConfigEnvInternal m)

-- | Authentication layer with token state and error handling,
--   wrapping the configuration layer.
type AuthEnv m = EitherT ErrorDesc (ReaderT B.ByteString (ConfigEnvInternal m))

-- | Submissions consist of a problem identifier and a set of file paths.
type Submission = (KattisProblem, [FilePath])

-- | Error description alias.
type ErrorDesc = B.ByteString

-- | Submissions are identified with an integer id.
type SubmissionId = Integer
--
-- | Problem sessions are identified with an integer id.
type ProblemSession = Integer

-- | Project-specific state consists of the problem name.
type ProjectState = (KattisProblem)

-- | Global configuration, initialized from the /.kattisrc/ file.
data ConfigState =
  ConfigState {
    -- | Username.
    user :: B.ByteString,
    -- | API token (hash).
    apiKey :: B.ByteString,
    -- | Host to be considered as service.
    host :: B.ByteString,
    -- | URL to login page, relative 'host'.
    loginPage :: B.ByteString,
    -- | URL to submit page, relative 'host'.
    submitPage :: B.ByteString,
    -- | Project-specific state, optionally loaded.
    project :: Maybe ProjectState
  }
  deriving Show

-- | A Kattis problem.
data KattisProblem
  -- | Problem ID, unique.
  = ProblemId Integer
  -- | Associated name of the problem.
  | ProblemName B.ByteString
  deriving (Eq, Show)

-- | Language used in submission.
data KattisLanguage
  -- | C++.
  = LangCplusplus
  -- | Java.
  | LangJava
  -- | C.
  | LangC
  -- | Haskell.
  | LangHaskell
  deriving (Eq, Show)

-- | Server response indicating successful login.
loginSuccess :: B.ByteString
loginSuccess = "Login successful"

-- | Extension of input test files.
inputTestExtension :: FilePath
inputTestExtension = ".in"

-- | Extension of reference ouput test files.
outputTestExtension :: FilePath
outputTestExtension = ".ans"

-- | Name of this program.
programName :: B.ByteString
programName = "katt"

-- | Relative path to project-specific configuration directory.
configDir :: B.ByteString
configDir = "." <> programName

-- | Relative path to folder containing tests.
testFolder :: FilePath
testFolder = "tests"

-- | URL to page with problem information, relative to 'host'.
problemAddress :: B.ByteString
problemAddress = "/problems/"

-- | Lift some error monad one layer.
unWrapTrans :: (Monad m, MonadTrans t) => EitherT e m a -> EitherT e (t m) a
unWrapTrans = EitherT . lift . runEitherT

-- | Execute an IO action and catch any exceptions.
tryIO :: MonadIO m => IO a -> EitherT ErrorDesc m a
tryIO = EitherT . liftIO . liftM (fmapL (B.pack . show)) . 
  (E.try :: (IO a -> IO (Either E.SomeException a)))

-- | Execute an IO action and catch any exceptions, tagged with description.
tryIOMsg :: MonadIO m => B.ByteString -> IO a -> EitherT ErrorDesc m a
tryIOMsg msg = EitherT . liftIO . liftM (fmapL $ const msg) . 
  (E.try :: (IO a -> IO (Either E.SomeException a)))

withConn :: MonadIO m => (Connection -> IO a) -> ConfigEnv m a
withConn f = do
  host' <- host <$> S.get
  conn <- tryIOMsg "Failed to establish connection" $ establishConnection host'
  tryIO $ E.finally (f conn) (closeConnection conn) 

-- | Evaluate an error action and terminate process upon failure.
terminateOnFailure :: MonadIO m => ErrorDesc -> EitherT ErrorDesc m a -> m a
terminateOnFailure msg state = do
  res <- runEitherT state
  liftIO $ case res of
    Left errorMessage -> do
      B.hPutStrLn stderr $ msg <> ", error: " <> errorMessage
      exitFailure
    Right success -> return success

-- | Sign an existing HTTP request with a temporary token.
makeSignedRequest :: RequestBuilder () -> AuthEnv IO Request
makeSignedRequest req = do
  key <- lift $ liftM (setHeader "Cookie") ask
  liftIO . buildRequest $ req >> key

-- | Default HTTP request.
defaultRequest :: RequestBuilder ()
defaultRequest = do
  setHeader "User-Agent" programName
  setHeader "Connection" "keep-alive"

-- | Retrieve a publically available page, using HTTP GET.
retrievePublicPage :: B.ByteString -> ConfigEnv IO B.ByteString
retrievePublicPage page = do
  header <- tryIO . buildRequest $ http GET page >> defaultRequest
  makeRequest header

-- | Retrieve a page requiring authentication, using HTTP GET.
retrievePrivatePage :: B.ByteString -> AuthEnv IO B.ByteString
retrievePrivatePage page = do
  header <- makeSignedRequest $ do
    http GET page
    defaultRequest
  unWrapTrans $ makeRequest header

-- | Make a HTTP request and retrieve the server response body.
makeRequest :: Request -> ConfigEnv IO B.ByteString
makeRequest header = withConn $ \conn -> do
  sendRequest conn header emptyBody
  receiveResponse conn concatHandler

-- | Extract correct temporary token from cookie header string.
extractSessionHeader :: B.ByteString -> Maybe B.ByteString
extractSessionHeader headerStr 
  | B.null match = Nothing
  | otherwise =
    case extractSessionHeader (B.tail match) of
      Just match' -> Just match'
      Nothing -> Just $ B.takeWhile (/= ';') match
  where
  (_, match) = B.breakSubstring "PHPSESSID" headerStr

-- | Authenticate an existing connection, returns a temporary token.
--   Basically the API token is used to acquire a session-specific token.
authenticate :: ConfigEnv IO B.ByteString
authenticate = do
  conf <- S.get

  header <- tryIO . buildRequest $ do
    http POST ("/" <> loginPage conf)
    defaultRequest
    setContentType "application/x-www-form-urlencoded"

  let formData = [("token", apiKey conf), ("user", user conf), ("script", "true")] 

  (headers, response) <- withConn $ \conn -> do
    sendRequest conn header $ encodedFormBody formData
    receiveResponse conn (\headers stream -> do
      response <- readExactly (B.length loginSuccess) stream
      return (headers, response))

  tryAssert ("Login failure. Server returned: '" <> response <> "'")
    (response == loginSuccess)
    
  noteT "Failed to parse login cookie" . hoistMaybe $ 
    getHeader headers "Set-Cookie" >>= extractSessionHeader

-- | Retrieve problem ID of a Kattis problem.
retrieveProblemId :: KattisProblem -> ConfigEnv IO Integer
retrieveProblemId (ProblemId id') = return id'
retrieveProblemId (ProblemName _) = undefined

-- | Retrieve problem name of a Kattis problem.
retrieveProblemName :: KattisProblem -> ConfigEnv IO B.ByteString
retrieveProblemName (ProblemId _) = undefined
retrieveProblemName (ProblemName name) = return name

