module Main where

import Control.Applicative ((<$>))
import Control.Monad
import Data.List (subsequences)
import Data.Monoid ((<>))
import System.Exit (exitFailure, exitSuccess)
import System.Directory (getDirectoryContents)
import System.IO
import Utils.Katt.SourceHandler
import Utils.Katt.Utils

main :: IO ()
main = do
  res <- verifyLanguages
  if res then exitSuccess else exitFailure

-- Given a set of source files, categorized into folders corresponding to all supported languages.
-- Test the following things:
-- (1) That the corresponding languages are actually identified correctly, also when intermixed.
-- (2) That all main method classes are identified correctly in the case of java and python

languages :: [(KattisLanguage, String)]
languages = [(LangCplusplus, "c++"), (LangC, "c"), (LangJava, "java")]

testDir :: String
testDir = "katt-tests/data/sample_submissions/"

verifyLanguages :: IO Bool
verifyLanguages = and <$> mapM (\lang -> liftM2 (&&) (verifyLanguage lang) (verifyMain lang)) languages
  where
  verifyMain info@(LangJava, folder) = do
    tests <- getFileNamesSimple $ testDir <> folder
    res <- mapM (\test -> findMainClass ([test], fst info)) tests
    if Nothing `notElem` res then return True else do
      hPutStrLn stderr $ "Failed to verify SourceHandler: main class for language " <> show (fst info)
      return False

  verifyMain _ = return True

  verifyLanguage (identifier, folder)= do
    tests <- getFileNames $ testDir <> folder
    let res = map determineLanguage tests
    if Nothing `notElem` res then return True else do
      hPutStrLn stderr $ "Failed to verify SourceHandler: language " <> show identifier
      return False

  filterNonFiles = filter (`notElem` [".", ".."])
  addFolder folder = map ((folder <> "/") <>)

  getFileNames folder = map (addFolder folder) <$> files
    where files = map filterNonFiles . drop 1 . subsequences
                          <$> getDirectoryContents folder

  getFileNamesSimple folder = addFolder folder <$> files
    where files = filterNonFiles <$> getDirectoryContents folder
