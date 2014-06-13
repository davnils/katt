{-# Language OverloadedStrings, ScopedTypeVariables #-}

--------------------------------------------------------------------
-- |
-- Module : Utils.Katt.Init
--
-- Init submodule providing initialization of problems
-- and entire problem sessions.
--
-- Problems are initialized by creating a directory, configuration file,
-- and optionally downloading all test files available.
-- Both zip-based test data and embedded HTML tables are supported.
--
-- Problem sessions are initialized by parsing the list of problems and
-- initializing each problem separately.

module Utils.Katt.Init
(initializeProblem, initializeSession)
where

import           Control.Applicative ((<$>), (<*))
import qualified Codec.Archive.Zip as Z
import           Control.Arrow ((***))
import           Control.Monad (liftM, liftM2, void, when)
import qualified Control.Monad.State as S
import           Control.Error hiding (tryIO)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable (fold)
import           Data.List (isSuffixOf, nub)
import           Data.Monoid ((<>))
import qualified System.Directory as D
import           System.IO (stderr)
import           Text.Parsec hiding (token)
import           Text.Parsec.ByteString
import qualified Utils.Katt.Configuration as C
import           Utils.Katt.Utils

-- | Parsed test cases associated with a problem.
type TestContent = [(B.ByteString, B.ByteString)]

-- | Possible test case scenarios.
data TestParser
  -- | No tests available.
  = NoTestsAvailable
  -- | Test content available in zip file, given as URL.
  | TestAddress B.ByteString
  -- | Embedded test content.
  | TestContents TestContent
  deriving Show

-- | Parse the possible different test file cases, given the problem page.
--   Any zip download links are preferred over embedded test data.
parseProblemPage :: B.ByteString -> TestParser
parseProblemPage contents = 
  case res of
    Left _ -> NoTestsAvailable
    Right test -> test
  where res = parse (try parseAddress <|> parseEmbedded) "Test parser" contents

-- | Try to parse a download URL from the supplied page data.
parseAddress :: GenParser Char st TestParser
parseAddress = do
  void $ manyTill anyChar (try $ startLink >> lookAhead endParser)
  TestAddress . B.cons '/' . B.pack <$> endParser

  where
  startLink = string "<a href='"
  endLink name = string ">" >> string name >> string "</a>"
  endParser = manyTill anyChar (char '\'') <* endLink "Download"

-- | Try to parse test cases embedded into HTML data.
--   Currently only table-style test cases are supported (e.g. problem 'friends').
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

-- | Retrieve the zip archive located at the specified URL and unzip the contents.
--   Matches input and output file pairs, producing a list of tuples.
downloadTestArchive :: B.ByteString -> ConfigEnv IO TestContent
downloadTestArchive url = do
  zipFile <- BL.fromChunks . return <$> retrievePublicPage url
  zipEntries <- tryIOMsg "Failed to unpack zip file: corrupt archive" $
    E.evaluate (Z.zEntries $ Z.toArchive zipFile)

  let filterFiles suffix = filter (isSuffixOf suffix . Z.eRelativePath) zipEntries
      inFiles = filterFiles inputTestExtension
      outFiles = filterFiles outputTestExtension

  tryAssert "Failed to unpack zip file: no test files found" $
    not (null zipEntries)
  tryAssert "Failed to unpack zip file: input and reference count doesn't match" $
    length inFiles == length outFiles

  return $ zipWith (curry convertEntry) inFiles outFiles
  where
  convertEntry = getData *** getData
  getData = fold . BL.toChunks . Z.fromEntry

-- | Retrieve test cases, which fall into either one of the three categories.
retrieveTestFiles :: KattisProblem -> ConfigEnv IO TestContent
retrieveTestFiles problem = do
  problemName <- tryIO $ retrieveProblemName problem
  problemPage <- retrievePublicPage $ problemAddress <> problemName

  case parseProblemPage problemPage of
    NoTestsAvailable -> do
      tryIO $ B.hPutStrLn stderr "No tests available"
      return []
    TestAddress addr -> downloadTestArchive addr
    TestContents list -> return list

-- | Page listing all problems associated with a problem session, relative 'Utils.host'.
sessionPage :: B.ByteString
sessionPage = "/standings/?sid="

-- | Parse a problem session page, locating all the associated problem names.
parseProblemList :: GenParser Char st [KattisProblem]
parseProblemList = skip >> endBy1 tag skip
  where
  beginLink = string "<a href='problems/"
  skip = manyTill anyChar (void (try beginLink) <|> eof)
  tag = do
    problem <- manyTill (letter <|> digit) (char '\'')
    return . ProblemName $ B.pack problem

-- | Given a problem session id, initialize all the corresponding problems.
initializeSession :: Bool -> ProblemSession -> ConfigEnv IO ()
initializeSession retrieveTests session = do
  contents <- retrievePublicPage $ sessionPage <> B.pack (show session)
  problems <- nub <$> (EitherT . return  . fmapL (B.pack . show) $ parse' contents)
  mapM_ (\problem -> do
      initializeProblem True retrieveTests problem
      restoreDir
    )
    problems
  where
  parse' = parse parseProblemList "Problem list parser"
  restoreDir = S.liftIO $ D.setCurrentDirectory ".."

-- | Given a problem identifier, setup directory structures and
--   optionally download test cases.
initializeProblem :: Bool -> Bool -> KattisProblem -> ConfigEnv IO ()
initializeProblem mkDir retrieveTests problem = do
  S.liftIO . putStrLn $ "Initializing problem: " <> show problem
  problemName <- tryIO $ retrieveProblemName problem
  tryIO . when mkDir $ do
    D.createDirectoryIfMissing False (B.unpack problemName)
    D.setCurrentDirectory (B.unpack problemName)

  tryIO $ D.createDirectoryIfMissing False (B.unpack configDir)

  fileExists <- S.liftIO C.projectConfigExists
  tryAssert
    "Project configuration file already exists, please remove it in order to continue."
    (not fileExists)

  S.modify $ \s -> s { project = Just $ ProblemName problemName }
  C.saveProjectConfig

  when retrieveTests $ do
    tryIO $ D.createDirectory testFolder
    files <- zip [1..] <$> retrieveTestFiles problem
    mapM_ (\(n :: Integer, (input, output)) -> do
      let fileName = testFolder <> "/" <> B.unpack problemName <> "-" <> show n
      tryIO $ B.writeFile (fileName <> inputTestExtension) input
      tryIO $ B.writeFile (fileName <>  outputTestExtension) output)
      files
