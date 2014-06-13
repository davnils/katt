{-# Language OverloadedStrings #-}

--------------------------------------------------------------------
-- |
-- Module : Utils.Katt.Upload
--
-- Upload submodule providing submissions of solutions and parsing of results.
--
-- A submission is done by including all recursively found files and filtering
-- using a file filter given as an argument.
-- This is followed by polling for a submission result until some final
-- submission state has been reached (e.g. accepted).
--
-- Currently multipart data upload is implemented since https-streams
-- (the HTTP client being used) does not support it (yet?).

module Utils.Katt.Upload
(makeSubmission)
where

import Control.Applicative ((<$>))
import qualified Utils.Katt.Configuration as C
import Control.Concurrent (threadDelay)
import Control.Error hiding (tryIO)
import Control.Lens
import Control.Monad.Reader
import qualified Control.Monad.State as S
import qualified Data.ByteString.Char8 as B
import Data.List ((\\), union, findIndex)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as WS
import Utils.Katt.SourceHandler
import Text.Parsec hiding (token)
import Text.Parsec.ByteString
import Utils.Katt.Utils

-- | Submission page URL, relative 'Utils.host', from which specific submission can be requested.
submissionPage :: B.ByteString
submissionPage = "submission"

-- | Possible states of a submission, with unknowns being grouped into 'Other'.
data SubmissionState
  -- | Submission is queued.
  = Queued
  -- | Submission is compiling.
  | Compiling
  -- | Submission is running.
  | Running
  -- | Wrong answer.
  | WrongAnswer
  -- | Time limit exceeded.
  | TimeLimitExceeded
  -- | Submission was accepted (only success state).
  | Accepted
  -- | Compile error.
  | CompileError
  -- | Run time error.
  | RunTimeError
  -- | Some other, unmatched error code. Only used when parsing fails.
  | Other
  deriving (Eq, Show)

-- | Possible states of a single test case, i.e. an (input, output) data pair.
data TestCase
  -- | Test case passed.
  = TestPassed
  -- | Test case failed (state /= Accepted)
  | TestFailed
  -- | Test case has not been executed.
  | NotTested
  deriving (Eq, Show)

-- | Check if a given state is final, i.e. won't transition into some other.
--   Note that 'Other' is listed as final.
finalSubmissionState :: SubmissionState -> Bool
finalSubmissionState s = elem s
  [WrongAnswer, TimeLimitExceeded, Accepted, CompileError, RunTimeError, Other]

-- | Make a submission of the project in the working directory.
--   Accepts a list of filters on the form /+file1 -file2 ../, which are
--   taken into account when locating all the source files.
--   /+file/ implies adding the specified file.
--   /-file/ implies removing the specified file.
--
--   In addition to the filters, all recursively found source code files
--   will be included in the submission.
makeSubmission :: [String] -> ConfigEnv IO ()
makeSubmission filterArguments = do
  exists <- liftIO C.projectConfigExists
  tryAssert "No project configuration could be found."
    exists

  C.loadProjectConfig
  problem <- fromJust <$> S.gets project
  conf <- S.get

  -- Locate all source files, filter based on filter list.
  files <- tryIOMsg "Failed to locate source files" findFiles
  let adjusted = adjust (parseFilter filterArguments) files

  liftIO $ mapM_ (putStrLn . ("Adding file: "++)) adjusted

  -- Authenticate, submit files, and retrieve submission id.
  let url = buildURL (host conf) (submitPage conf)
      toState sess = (sess, host conf)

  submission <- withAuth $ \sess ->
    submitSolution (toState sess) url (problem, adjusted)

  tryIO $ do
    putStrLn $ "Made submission: " <> show submission
    threadDelay initialTimeout

  withAuth $ \sess ->
    checkSubmission (toState sess) submission
  where
  adjust Nothing files = files
  adjust (Just (add, sub)) files = union (files \\ sub) add

  -- Initial timeout before requesting updates is 2 s.
  initialTimeout = 2000000

-- | Poll kattis for updates on a submission.
--  This function returns when the submission has reached one of the final states.
--  TODO: Consider exponential back-off and timeout
checkSubmission :: Session -> SubmissionId -> EitherT ErrorDesc IO ()
checkSubmission sess submission = do
  page <- retrievePrivatePage sess $
    submissionPage <> "?id=" <> B.pack (show submission)
  let (state, tests) = parseSubmission page

  if finalSubmissionState state
    then
      tryIO $ printResult tests state
    else do
      tryIO $ putStrLn "Waiting for completion.." >> threadDelay interval
      checkSubmission sess submission
   
  where
  -- Default poll interval is 1 s.
  interval = 1000000

-- | Parse the supplied submission page into:
--   (1) Current submission state
--   (2) Status of all test cases
parseSubmission :: B.ByteString -> (SubmissionState, [TestCase])
parseSubmission contents =
  case res of
    Left err' -> error $ "Internal parser error" <> show err'
    Right res' -> res'
  where
  res = parse parser "Submission parser" contents
  parser = liftM2 (,) parseStatus parseTestCases

-- | String separator parser.
strSep :: GenParser Char st ()
strSep = void (char '\'' <|> char '"')

-- | End-of-tag parser, ignores everything up to the end of the current tag.
endTag :: GenParser Char st ()
endTag = void $ manyTill anyChar (char '>')

-- | Parse the submission status field, beginning from any offset in the page data.
parseStatus :: GenParser Char st SubmissionState
parseStatus = skip >> status
  where
  beginStatus = do
    void $ string "<td class="
    strSep >> string "status" >> strSep >> endTag
    void $ string "<span class=" >> strSep

  -- Skip to the appropiate <td> tag.
  skip = manyTill anyChar (void (try beginStatus) <|> eof)

  -- Parse contents in <td>...</td>.
  -- TODO: check if manyTill can be rewritten to the endTag pattern
  status = do
    void $ manyTill anyChar strSep
    endTag
    statusStr <- manyTill (letter <|> space) (char '<')
    return $ conv statusStr

  conv "Time Limit Exceeded" = TimeLimitExceeded
  conv "Wrong Answer" = WrongAnswer
  conv "Accepted" = Accepted
  conv "Memory Limit Exceeded" = Other
  conv "Compiling" = Compiling
  conv "Running" = Running
  conv "Compile Error" = CompileError
  conv "Run Time Error" = RunTimeError
  conv _ = Other

-- | Parse the status of all test cases, beginning from any offset in the page data.
--   May return zero test cases when a submission fails
--   with certain status values, e.g. /Compile Error/.
parseTestCases :: GenParser Char st [TestCase] 
parseTestCases = skip >> tests
  where
  beginTests = do
    void $ string "<div class="
    strSep >> string "testcases" >> strSep
    endTag

  -- Locate surrounding div tag.
  skip = manyTill anyChar (void (try beginTests) <|> eof)

  -- Parse all test cases.
  tests = many testCase

  -- Each test case is basically <span [class="status"]>...</span> 
  -- where a missing class attribute implies that it hasn't been executed.
  testCase = do
    void . try $ string "<span "
    classResult <- optionMaybe $ do
      string "class=" >> strSep
      manyTill anyChar strSep

    void . manyTill anyChar $ string "</span>"
    fromMaybe (return NotTested) (mapResult <$> classResult)

  mapResult "accepted" = return TestPassed
  mapResult "rejected" = return TestFailed
  mapResult _ = parserZero

-- | Print the result of a submission.
--   Will also take care of the special case when no test cases were parsed.
printResult :: [TestCase] -> SubmissionState -> IO ()
printResult tests state
  | state == Accepted = putStrLn $ "Accepted, " <> numTests <> " test(s) passed."
  | null tests = putStrLn resultStr
  | otherwise = putStrLn $ resultStr <> testCaseStr
  where
  numTests = show $ length tests
  firstFailed = show . (+1) . fromMaybe 0 $ findIndex (/= TestPassed) tests
  resultStr = "Result: " <> show state
  testCaseStr = ", failed on test case " <> firstFailed <> " of " <> numTests

-- | Submit a solution, given problem name and source code files.
submitSolution :: Session -> String -> Submission -> EitherT ErrorDesc IO SubmissionId
submitSolution (sess, _) url (problem, files) = do
  -- Determine language in submission.
  language <- noteT ("\nFailed to decide submission language\n" <>
                    "Please use either Java or some union of C++ and C")
    . hoistMaybe $ determineLanguage files
  let languageStr = languageKattisName language

  -- Locate main class, if any
  mainClassStr <- join . liftIO $
    (noteT "Failed to locate the \"public static void main\" method - is there any?" . hoistMaybe)
      <$> findMainClass (files, language)

  -- Construct POST data
  problemName <- tryIO $ retrieveProblemName problem
  let files'     = map (W.partFile "sub_file[]") files
      conv       = T.pack . B.unpack
      postFields = [W.partText "submit" "true"]
                <> [W.partText "submit_ctr" "2"]
                <> [W.partText "language" (conv languageStr)]
                <> [W.partText "mainclass" (T.pack mainClassStr)]
                <> [W.partText "problem" (conv problemName)]
                <> [W.partText "tag" ""]
                <> [W.partText "script" "true"]

  -- Submit the request
  reply <- tryIO $ WS.postWith
    defaultOpts
    sess
    url
    (files' <> postFields)

  -- Extract the submission ID
  let body = reply ^. W.responseBody
  (EitherT . return  . fmapL (B.pack . show)) $
    parse parseSubmissionId "Submission ID parser" body

  where
  parseSubmissionId = manyTill anyChar (lookAhead identifier) >> identifier
  identifier = read <$> many1 digit
