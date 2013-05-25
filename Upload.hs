{-# Language OverloadedStrings, ScopedTypeVariables #-}

module Upload (makeSubmission) where

import Control.Applicative ((<$>))
import Blaze.ByteString.Builder (fromByteString)
import qualified Configuration as C
import Control.Concurrent (threadDelay)
import Control.Error hiding (tryIO)
import Control.Monad.Reader
import qualified Control.Monad.State as S
import qualified Data.ByteString.Char8 as B
import Data.List ((\\), union, findIndex)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Network.Http.Client
import SourceHandler
import System.IO.Streams (write)
import Text.Parsec hiding (token)
import Text.Parsec.ByteString
import Text.Regex.Posix
import Utils

crlf :: B.ByteString
crlf = "\r\n"

data MultiPartField
  = Option [B.ByteString] B.ByteString
  | File FilePath

buildChunk :: MultiPartField -> IO B.ByteString
buildChunk (File path) = do
  file <- B.readFile path
  return $ B.intercalate crlf [headerLine, "Content-Type: text/x-c++src", "", file, ""]
  where
    headerLine = B.intercalate "; " ["Content-Disposition: form-data", "name=\"sub_file[]\"",
                                     B.concat ["filename=\"", B.pack path, "\""]]

buildChunk (Option fields payload) = return $ B.intercalate crlf [headerLine, "", payload, ""]
  where
    headerLine = B.intercalate "; " fieldList
    fieldList = "Content-Disposition: form-data" : fields

submissionPage :: B.ByteString
submissionPage = "submission"

data SubmissionState
  = Queued
  | Compiling
  | Running
  | WrongAnswer
  | TimeLimitExceeded
  | Accepted
  | CompileError
  | Other
  deriving (Eq, Show)

data TestCase
  = TestPassed
  | TestFailed
  | NotTested
  deriving (Eq, Show)

finalSubmissionState :: SubmissionState -> Bool
finalSubmissionState s = elem s
  [WrongAnswer, TimeLimitExceeded, CompileError, Accepted, Other]

makeSubmission :: [String] -> ConnEnv IO ()
makeSubmission filterArguments = do
  exists <- liftIO C.projectConfigExists
  tryAssert "No project configuration could be found."
    exists

  unWrapTrans C.loadProjectConfig
  problem <- lift . lift $ (fromJust <$> S.gets project)

  files <- tryIOMsg "Failed to locate source files" findFiles
  let adjusted = adjust (parseFilter filterArguments) files

  liftIO $ mapM_ (putStrLn . ("Adding file: "++)) adjusted

  token <- authenticate 
  submission <- EitherT $ runReaderT
    (runEitherT $ submitSolution (problem, adjusted)) token

  tryIO . putStrLn $ "Made submission: " <> show submission
  tryIO $ threadDelay initialTimeout
  reestablishConnection

  token' <- authenticate 
  EitherT $ runReaderT
    (runEitherT $ checkSubmission submission) token'

  where
  adjust Nothing files = files
  adjust (Just (add, sub)) files = union (files \\ sub) add
  initialTimeout = 2000000

-- | Poll kattis for updates on a submission.
--  This function returns when the submission has reached one of the final states.
--  TODO: Consider exponential back-off and timeout
checkSubmission :: SubmissionId -> AuthEnv IO ()
checkSubmission submission = do
  page <- retrievePrivatePage $ "/" <> submissionPage <> "?id=" <> B.pack (show submission)
  let (state, tests) = parseSubmission page

  if finalSubmissionState state
    then
      liftIO $ printResult tests state
    else do
      tryIO $ putStrLn "Waiting for completion.." >> threadDelay interval
      checkSubmission submission
      unWrapTrans reestablishConnection
   
  where
  interval = 1000000

parseSubmission :: B.ByteString -> (SubmissionState, [TestCase])
parseSubmission contents =
  case res of
    Left _ -> error "Internal parser error"
    Right res' -> res'
  where
  res = parse parser "Submission parser" contents
  parser = do
    status <- parseStatus
    tests <- parseTestCases
    return (status, tests)

parseStatus :: GenParser Char st SubmissionState
parseStatus = skip >> status
  where
  beginStatus = string "<td class='status'><span class=\""
  skip = manyTill anyChar (void (try beginStatus) <|> eof)
  status = do
    _ <- manyTill (letter) (char '\"')
    _ <- char '>'
    statusStr <- manyTill (letter <|> space) (char '<')
    return $ conv statusStr

  conv "Time Limit Exceeded" = TimeLimitExceeded
  conv "Wrong Answer" = WrongAnswer
  conv "Accepted" = Accepted
  conv "Memory Limit Exceeded" = Other
  conv "Compiling" = Compiling
  conv "Running" = Running
  conv "Compile Error" = CompileError
  conv _ = Other

-- | Extract all span tags within the testcases div.
-- for each span tag: check the corresponding class
-- either 'rejected' or 'accepted'.
parseTestCases :: GenParser Char st [TestCase] 
parseTestCases = skip >> tests
  where
  beginTests = string "<div class='testcases'>"
  endTests = string "</div>"
  skip = manyTill anyChar (void (try beginTests) <|> eof)

  tests = manyTill testCase (void (try endTests) <|> eof)
  testCase = do
    _ <- string "<span "
    let classParser = try $ string "class="
    skipMany (notFollowedBy classParser >> param (manyTill anyChar (char '=')))
    (_, result) <- param classParser
    _ <- manyTill anyChar (char '>' >> manyTill anyChar (string "</span>"))

    mapResult result

  mapResult "accepted" = return TestPassed
  mapResult "rejected" = return TestFailed
  mapResult _ = parserZero

  param keyParser = do
    tag <- keyParser
    val <- char '\'' >> manyTill anyChar (char '\'')
    return (tag, val)

printResult :: [TestCase] -> SubmissionState -> IO ()
printResult tests state
  | state == Accepted = putStrLn $ "Accepted, " <> numTests <> " test(s) passed."
  | otherwise = putStrLn $ "Result: " <> show state <> ", failed on test case " <>
                           firstFailed <> " of " <> numTests
  where
  numTests = show $ length tests
  firstFailed = show . (+1) . fromMaybe 0 $ findIndex (/= TestPassed) tests

submitSolution :: Submission -> AuthEnv IO SubmissionId
submitSolution (problem, files) = do
  let multiPartSeparator = "separator"

  conf <- lift . lift $ lift S.get
  header <- makeSignedRequest $ do
    http POST ("/" <> submitPage conf)
    defaultRequest
    setContentType $ B.append "multipart/form-data; boundary=" multiPartSeparator

  problemName <- unWrapTrans $ retrieveProblemName problem

  let postFields = [Option ["name=\"submit\""] "true"]
                <> [Option ["name=\"submit_ctr\""] "2"]
                <> [Option ["name=\"language\""] "C++"]
                <> [Option ["name=\"mainclass\""] ""]
                <> [Option ["name=\"problem\""] problemName]
                <> [Option ["name=\"tag\""] ""]
                <> [Option ["name=\"script\""] "true"]
                <> map File files

  conn <- lift . lift $ S.get
  tryIO $ sendRequest conn header (\o -> do
    mapM_ (\part -> do
        serialized <- buildChunk part
        write (Just . fromByteString $ B.concat ["--", multiPartSeparator, crlf, serialized]) o)
      postFields

    write (Just . fromByteString $ B.concat ["--", multiPartSeparator, "--", crlf]) o
    )

  reply <- tryIO $ receiveResponse conn concatHandler
  tryRead "Failed to parse submission id from server" . B.unpack $ reply =~ ("[0-9]+" :: B.ByteString)
