{-# Language OverloadedStrings, ScopedTypeVariables,
    NoMonomorphismRestriction #-}

module Utils where

import Codec.Archive.Zip
import Control.Applicative ((<$>), (<*))
import Control.Error hiding (tryIO)
import qualified Control.Exception as E
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable (fold)
import Data.List (find, isSuffixOf)
import Data.Monoid ((<>))
import Network.Http.Client
import System.Exit (exitFailure)
import System.Directory
import System.IO (stderr)
import System.IO.Streams (readExactly)
import Text.Parsec hiding (token)
import Text.Parsec.ByteString

type ConfigEnv m = ReaderT ConfigState m
type ConnEnvInternal m = ReaderT Connection (ConfigEnv m)
type ConnEnv m = EitherT ErrorDesc (ConnEnvInternal m)
type AuthEnv m = EitherT ErrorDesc (ReaderT B.ByteString (ConnEnvInternal m))

type Submission = (KattisProblem, [FilePath])
type ErrorDesc = B.ByteString
type SubmissionId = Integer
type ProjectState = (KattisProblem)

data ConfigState = ConfigState {
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
  deriving Show

loginSuccess :: B.ByteString
loginSuccess = "Login successful"

inputTestExtension :: FilePath
inputTestExtension = ".in"

outputTestExtension :: FilePath
outputTestExtension = ".ans"

configDir :: B.ByteString
configDir = ".sofie"

testFolder :: FilePath
testFolder = "tests"

problemAddress :: B.ByteString
problemAddress = "/problems/"

noAuth :: (Monad m, MonadTrans t) => EitherT e m a -> EitherT e (t m) a
noAuth = EitherT . lift . runEitherT

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

authenticate :: ConnEnv IO B.ByteString
authenticate = do
  page <- lift . lift $ asks loginPage
  header <- tryIO . buildRequest $ do
    http POST ("/" <> page)
    defaultRequest
    setContentType "application/x-www-form-urlencoded"

  conf <- lift $ lift ask

  let formData = [("token", apiKey conf), ("user", user conf), ("script", "true")] 
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

-- | Parse the three different test file cases.
parseProblemPage :: B.ByteString -> TestParser
parseProblemPage contents = 
  case res of
    Left _ -> NoTestsAvailable
    Right test -> test
  where res = parse (try parseAddress <|> parseEmbedded) "Test parser" contents

-- | Try to parse a download URL from the supplied data.
-- | format: "<a href='download/sampledata?id=problem'>Download</a>"
parseAddress :: GenParser Char st TestParser
parseAddress = do
  void $ manyTill anyChar (try $ startLink >> lookAhead endParser)
  TestAddress . (B.cons '/') . B.pack <$> endParser

  where
  startLink = string "<a href='"
  endLink name = string ">" >> string name >> string "</a>"
  endParser = manyTill anyChar (char '\'') <* endLink "Download"

-- | Try to parse test cases embedded into HTML data.
-- | Currently only table-style test cases are supported (e.g. problem 'friends').
parseEmbedded :: GenParser Char st TestParser
parseEmbedded = TestContents <$> tests 
  where
  sp = skipMany $ space <|> newline <|> tab
  beginTag tag = void $ char '<' >> sp >> string tag >> sp >> char '>'
  endTag tag = void $ char '<' >> sp >> char '/' >> string tag >> sp >> char '>'
  htmlTag tag p = do
    sp >> beginTag tag >> sp
    manyTill p $ try $ endTag tag

  tr = htmlTag "tr"
  td = htmlTag "td"
  pre = htmlTag "pre"

  tests = manyTill anyChar (lookAhead startTable) >> endBy1 testTable sp

  startTable = void . try $ string "<table class=\"sample\" summary=\"sample data\">"
  endTable  = void $ string "</table>"

  testTable = do
    startTable
    inner <- tableBody
    sp >> endTable
    return inner

  tableBody = do
    void $ tr anyChar
    try (fold <$> tr testCase) <* sp

  innerTestData = do
    sp
    B.pack <$> pre anyChar <* sp

  testData = liftM fold $ td innerTestData <* sp
  testCase = liftM2 (,) testData testData

-- | Retrieve the zip archive located at the specified URL
-- | and unzip the contents.
downloadTestArchive :: B.ByteString -> ConnEnv IO TestContent
downloadTestArchive url = do
  zipFile <- BL.fromChunks . return <$> retrievePublicPage url
  zipEntries <- tryIOMsg "Failed to unpack zip file: corrupt archive" $
    E.evaluate (zEntries $ toArchive zipFile)

  let filterFiles suffix = filter (isSuffixOf suffix . eRelativePath) zipEntries
      inFiles = filterFiles inputTestExtension
      outFiles = filterFiles outputTestExtension

  tryAssert "Failed to unpack zip file: no test files found" $
    length zipEntries > 0
  tryAssert "Failed to unpack zip file: input and reference count doesn't match" $
    length inFiles == length outFiles

  return $ map convertEntry $ zip inFiles outFiles
  where
  convertEntry entry = (getData $ fst entry, getData $ snd entry)
  getData = fold . BL.toChunks . fromEntry

  -- should Partition files into in and ans files, then match them
  -- check that there are an equivalent amount of each

-- | Retrieve test files, which fall into one or several of the following categories:
-- | (1) nonexistent
-- | (2) embedded on problem page
-- | (3) zip file linked from problem page
retrieveTestFiles :: KattisProblem -> ConnEnv IO TestContent
retrieveTestFiles problem = do
  problemName <- retrieveProblemName problem
  problemPage <- retrievePublicPage $ problemAddress <> problemName

  -- determine which of the three cases apply to this problem
  case parseProblemPage problemPage of
    NoTestsAvailable -> do
      tryIO $ B.hPutStrLn stderr "No tests available"
      return []
    TestAddress addr -> downloadTestArchive addr
    TestContents list -> return list

initializeProblem :: KattisProblem -> Bool -> Bool -> ConnEnv IO ()
initializeProblem problem mkDir retrieveTests = do
  problemName <- retrieveProblemName problem
  tryIO . when mkDir $ do
    createDirectoryIfMissing False (B.unpack problemName)
    setCurrentDirectory (B.unpack problemName)

  tryIO $ createDirectoryIfMissing False (B.unpack configDir)
  when retrieveTests $ do
    tryIO $ createDirectory testFolder
    files <- zip [1..] <$> retrieveTestFiles problem
    mapM_ (\(n :: Integer, (input, output)) -> do
      let fileName = testFolder <> "/" <> B.unpack problemName <> "-" <> show n
      tryIO $ B.writeFile (fileName <> inputTestExtension) input
      tryIO $ B.writeFile (fileName <>  outputTestExtension) output)
      files
