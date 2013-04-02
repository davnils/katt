{-# Language OverloadedStrings, ScopedTypeVariables,
    NoMonomorphismRestriction #-}

module Utils where

import Control.Applicative ((<$>))
import Control.Error hiding (tryIO)
import qualified Control.Exception as E
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
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

-- TODO: verify usage of 'try'
parseEmbedded :: GenParser Char st TestParser
parseEmbedded = between startTag endTag tableBody
  where
  startTag = string "<table class=\"sample\" summary=\"sample data\">"
  endTag = string "</table>"

  htmlTag :: GenParser Char st () -> GenParser Char st' B.ByteString -> GenParser Char st'' [B.ByteString]
  htmlTag tag p = undefined {- do
    beginHtml tag
    manyTill p $ try $ endHtml tag -}

  {- htmlTag' :: Parser a -> Parser (B.ByteString, B.ByteString) -> Parser (B.ByteString, B.ByteString)
  htmlTag' tag p = do
    beginHtml tag
    content <- p
    endHtml tag
    return content 

  beginHtml tag = char '<' >> spaces >> tag >> spaces >> char '>' >> return ()
  endHtml tag = char '<' >> spaces >> char '/' >> tag >> spaces >> char '>' >> return () -}
  tr :: GenParser Char st B.ByteString -> GenParser Char st' [B.ByteString]
  trTag :: GenParser Char st ()
  trTag = char 't' >> char 'r' >> return ()
  tr f = htmlTag trTag f
  td = htmlTag (string "td" >> return ())
  pre = htmlTag (string "pre" >> return ())
  tableBody = do
    tr anyChar
    return $ TestContents [("", "")]
    -- TestContents <$> many1 (htmlTag' (string "tr") testCase)
  {- testCaseWrapper = td $ spaces >> newline >> pre anyToken
  testCase = liftM2 (,) testCaseWrapper testCaseWrapper -}

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
