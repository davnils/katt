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

import           Control.Error hiding (tryIO)
import qualified Control.Exception as E
import           Control.Lens
import           Control.Monad (liftM)
import           Control.Monad.Trans (lift)
import qualified Control.Monad.State as S
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid ((<>))
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as WS
import           System.Exit (exitFailure)
import           System.IO (stderr)

-- | Configuration layer consisting of configuration state.
type ConfigEnvInternal m = S.StateT ConfigState m
-- | Configuration layer wrapped with error handling.
type ConfigEnv m = EitherT ErrorDesc (ConfigEnvInternal m)

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

-- | HTTP client session and the host path.
type Session = (WS.Session, B.ByteString)

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
unWrapTrans :: (Monad m, S.MonadTrans t) => EitherT e m a -> EitherT e (t m) a
unWrapTrans = EitherT . lift . runEitherT

-- | Execute an IO action and catch any exceptions.
tryIO :: S.MonadIO m => IO a -> EitherT ErrorDesc m a
tryIO = EitherT . S.liftIO . liftM (fmapL (B.pack . show)) .
  (E.try :: (IO a -> IO (Either E.SomeException a)))

-- | Execute an IO action and catch any exceptions, tagged with description.
tryIOMsg :: S.MonadIO m => B.ByteString -> IO a -> EitherT ErrorDesc m a
tryIOMsg msg = EitherT . S.liftIO . liftM (fmapL $ const msg) .
  (E.try :: (IO a -> IO (Either E.SomeException a)))

-- | Evaluate an error action and terminate process upon failure.
terminateOnFailure :: S.MonadIO m => ErrorDesc -> EitherT ErrorDesc m a -> m a
terminateOnFailure msg state = do
  res <- runEitherT state
  S.liftIO $ case res of
    Left errorMessage -> do
      B.hPutStrLn stderr $ msg <> ", error: " <> errorMessage
      exitFailure
    Right success -> return success

-- | Default HTTP options.
defaultOpts :: W.Options
defaultOpts = W.defaults
            & W.header "User-Agent" .~ [programName]

-- | Retrieve a publicly available page, using HTTP GET.
retrievePublicPage :: B.ByteString -> ConfigEnv IO B.ByteString
retrievePublicPage path = do
  host' <- S.gets host
  reply <- tryIO $ W.getWith defaultOpts $ buildURL host' path
  return . B.concat . BL.toChunks $ reply ^. W.responseBody

-- | Retrieve a page requiring authentication, using HTTP GET.
retrievePrivatePage :: Session -> B.ByteString -> EitherT ErrorDesc IO B.ByteString
retrievePrivatePage (sess, host') page = do
  reply <- tryIO $ WS.getWith defaultOpts sess (buildURL host' page)
  return . B.concat . BL.toChunks $ reply ^. W.responseBody

-- | Construct URL from host path (e.g. /http:\/\/x.com\/) and path (e.g. //).
buildURL :: B.ByteString -> B.ByteString -> String
buildURL host' path = B.unpack $ host' <> "/" <> path

-- | Authenticate and run the provided action.
withAuth :: (WS.Session -> EitherT ErrorDesc IO a) -> ConfigEnv IO a
withAuth action = do
  conf <- S.get

  EitherT . S.liftIO . WS.withSession $ \sess -> runEitherT $ do
    let formData = [("token" :: B.ByteString, apiKey conf),
                    ("user", user conf),
                    ("script", "true")]
        url      = buildURL (host conf) (loginPage conf)

    reply <- tryIO $ WS.postWith defaultOpts sess url formData
    let response = B.concat . BL.toChunks $ reply ^. W.responseBody

    tryAssert ("Login failure. Server returned: '" <> response <> "'")
      (response == loginSuccess)

    action sess

-- | Retrieve problem ID of a Kattis problem.
retrieveProblemId :: KattisProblem -> IO Integer
retrieveProblemId (ProblemId id') = return id'
retrieveProblemId (ProblemName _) = undefined

-- | Retrieve problem name of a Kattis problem.
retrieveProblemName :: KattisProblem -> IO B.ByteString
retrieveProblemName (ProblemId _) = undefined
retrieveProblemName (ProblemName name) = return name
