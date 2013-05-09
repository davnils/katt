{-# Language OverloadedStrings, ScopedTypeVariables,
    NoMonomorphismRestriction #-}

module Init (initializeProblem, initializeSession) where

import Control.Applicative ((<$>), (<*))
import Codec.Archive.Zip
import Control.Arrow ((***))
import qualified Control.Monad.State as S
import qualified Configuration as C
import Control.Error hiding (tryIO)
import qualified Control.Exception as E
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable (fold)
import Data.List (isSuffixOf, nub)
import Data.Monoid ((<>))
import System.Directory
import System.IO (stderr)
import Text.Parsec hiding (token)
import Text.Parsec.ByteString
import Utils

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
  TestAddress . B.cons '/' . B.pack <$> endParser

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
    not (null zipEntries)
  tryAssert "Failed to unpack zip file: input and reference count doesn't match" $
    length inFiles == length outFiles

  return $ zipWith (curry convertEntry) inFiles outFiles
  where
  convertEntry = getData *** getData
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

sessionPage :: B.ByteString
sessionPage = "/standings/?sid="

parseProblemList :: GenParser Char st [KattisProblem]
parseProblemList = skip >> endBy1 tag skip
  where
  beginLink = string "<a href='problems/"
  skip = manyTill anyChar (void (try beginLink) <|> eof)
  tag = do
    problem <- manyTill (letter <|> digit) (char '\'')
    return . ProblemName $ B.pack problem

-- Given a problem session id, call initializeProblem
-- for each of the problems available, or return with none available.
initializeSession :: Bool -> ProblemSession -> ConnEnv IO ()
initializeSession retrieveTests session = do
  contents <- retrievePublicPage $ sessionPage <> (B.pack $ show session)
  problems <- nub <$> (EitherT . return  . fmapL (B.pack . show) $ parse' contents)
  mapM_ (\problem -> initializeProblem True retrieveTests problem >> restoreDir) problems
  where
  parse' contents = parse (parseProblemList) "Problem list parser" contents
  restoreDir = liftIO $ setCurrentDirectory ".."
  -- TODO: restore to previously used directory

-- Given a problem identifier, setup directory structures and
-- optionally download test cases.
initializeProblem :: Bool -> Bool -> KattisProblem -> ConnEnv IO ()
initializeProblem mkDir retrieveTests problem = do
  liftIO . putStrLn $ "Initializing problem: " <> show problem
  problemName <- retrieveProblemName problem
  tryIO . when mkDir $ do
    createDirectoryIfMissing False (B.unpack problemName)
    setCurrentDirectory (B.unpack problemName)

  tryIO $ createDirectoryIfMissing False (B.unpack configDir)

  fileExists <- liftIO C.projectConfigExists
  tryAssert
    "Project configuration file already exists, please remove it in order to continue."
    (not fileExists)

  lift . S.modify $ \s -> s { project = Just $ ProblemName problemName }
  C.saveProjectConfig

  when retrieveTests $ do
    tryIO $ createDirectory testFolder
    files <- zip [1..] <$> retrieveTestFiles problem
    mapM_ (\(n :: Integer, (input, output)) -> do
      let fileName = testFolder <> "/" <> B.unpack problemName <> "-" <> show n
      tryIO $ B.writeFile (fileName <> inputTestExtension) input
      tryIO $ B.writeFile (fileName <>  outputTestExtension) output)
      files
