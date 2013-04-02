{-# Language OverloadedStrings, ScopedTypeVariables,
    NoMonomorphismRestriction #-}

module Utils where

import Control.Applicative ((<$>))
import Control.Error hiding (tryIO)
import qualified Control.Exception as E
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import Data.Foldable (fold)
import Data.List (find)
import Data.Monoid ((<>))
import Network.Http.Client
import System.Exit (exitFailure)
import System.Directory
import System.IO (stderr)
import System.IO.Streams (readExactly)
import Text.Parsec hiding (token)
import Text.Parsec.ByteString

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

configDir :: B.ByteString
configDir = ".sofie"

inputTestExtension :: B.ByteString
inputTestExtension = ".in"

outputTestExtension :: B.ByteString
outputTestExtension = ".ans"

problemAddress :: B.ByteString
problemAddress = "problems/"

noAuth :: (Monad m, MonadTrans t) => EitherT e m a -> EitherT e (t m) a
noAuth = EitherT . lift . runEitherT

tryIO :: MonadIO m => IO a -> EitherT ErrorDesc m a
tryIO = EitherT . liftIO . liftM (fmapL (B.pack . show)) . 
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

retrievePublicPage :: B.ByteString -> ConnEnv IO B.ByteString
retrievePublicPage page = do
  header <- tryIO . buildRequest $ http GET page >> defaultRequest
  makeRequest header

retrievePrivatePage :: B.ByteString -> AuthEnv IO B.ByteString
retrievePrivatePage page = do
  header <- makeSignedRequest $ do
    http GET page
    defaultRequest
  noAuth $ makeRequest header

makeRequest :: Request -> ConnEnv IO B.ByteString
makeRequest header = do
  conn <- lift ask
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

  tryAssert ("Login failure. Server returned: '" <> response <> "'")
    (response == loginSuccess)
    
  noteT "Failed to parse login cookie" . hoistMaybe $ do
    cookies <- B.words <$> getHeader headers "Set-Cookie"
    B.takeWhile (/= ';') <$> find ("PHPSESSID" `B.isPrefixOf`) cookies

retrieveProblemId :: KattisProblem -> ConnEnv IO Integer
retrieveProblemId (ProblemId id') = return id'
retrieveProblemId (ProblemName _) = undefined

retrieveProblemName :: KattisProblem -> ConnEnv IO B.ByteString
retrieveProblemName (ProblemId _) = undefined
retrieveProblemName (ProblemName name) = return name

type TestContent = [(B.ByteString, B.ByteString)]

data TestParser
  = NoTestsAvailable
  | TestAddress B.ByteString
  | TestContents TestContent
  deriving Show

-- parse the three different test file cases
parseProblemPage :: B.ByteString -> TestParser
parseProblemPage contents = 
  case res of
    Left failure -> NoTestsAvailable
    Right test -> test
  where res = parse (try parseAddress <|> try parseEmbedded) "Test parser" contents

-- TODO: Extract any existing address
-- "<a href='download/sampledata?id=friends'>Download</a>"
parseAddress :: GenParser Char st TestParser
parseAddress = undefined

-- TODO: (1) fix handling of newline and spaces (2) verify usage of 'try'
parseEmbedded :: GenParser Char st TestParser
parseEmbedded = between startEmbedded endEmbedded tableBody
  where
  startEmbedded = string "<table class=\"sample\" summary=\"sample data\">"
  endEmbedded  = string "</table>"

  htmlTag tag p = do
    sp >> beginTag tag >> sp
    manyTill p $ try $ endTag tag

  sp = skipMany $ skipMany $ space <|> newline <|> tab
  beginTag tag = char '<' >> spaces >> tag >> spaces >> char '>'
  endTag tag = char '<' >> spaces >> char '/' >> tag >> spaces >> char '>'

  tr = htmlTag $ string "tr"
  td = htmlTag $ string "td"
  pre = htmlTag $ string "pre"

  tableBody = do
    tr anyChar
    TestContents <$> many1 (fold <$> htmlTag (string "tr") testCase)

  testData = liftM fold $ td $ do
    spaces
    newline
    beginTag $ string "pre"
    B.pack <$> manyTill anyChar (try $ endTag $ string "pre")

  testCase = liftM2 (,) testData testData

-- retrieve test files, either
-- (1) nonexistent
-- (2) embedded on problem page
-- (3) linked from problem page 
retrieveTestFiles :: KattisProblem -> ConnEnv IO TestContent
retrieveTestFiles problem = do
  problemName <- retrieveProblemName problem
  problemPage <- retrievePublicPage $ problemAddress <> problemName

  -- determine which of the three cases apply to this problem
  case parseProblemPage problemPage of
    NoTestsAvailable -> do
      tryIO $ B.hPutStrLn stderr "No tests available"
      return []
    TestAddress addr -> undefined
    TestContents list -> return list

initializeProblem :: KattisProblem -> Bool -> Bool -> ConnEnv IO ()
initializeProblem problem mkDir retrieveTests = do
  problemName <- retrieveProblemName problem
  tryIO . when mkDir $ do
    createDirectoryIfMissing False (B.unpack problemName)
    setCurrentDirectory (B.unpack problemName)

  tryIO $ createDirectoryIfMissing False (B.unpack configDir)
  when retrieveTests $ do
    files <- zip [1..] <$> retrieveTestFiles problem
    mapM_ (\(n :: Integer, (input, output)) -> do
      let fileName = B.unpack $ problemName <> "-" <> B.pack (show n)
      tryIO $ B.writeFile (fileName <> ".in") input
      tryIO $ B.writeFile (fileName <> ".ans") output)
      files
