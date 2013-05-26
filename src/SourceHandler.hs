{-# Language OverloadedStrings #-}

--------------------------------------------------------------------
-- |
-- Module : SourceHandler
--
-- Provides searching of source code files and language identification.
--
-- Language identification is required in order to detect any
-- inconsistencies (e.g. combining Java and C), and to tag submissions.
--
-- Java also requires identifying which file contains the Main class.
--

module SourceHandler (parseFilter, findFiles, determineLanguage, findMainClass) where

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.List
import System.Directory
import System.IO
import Utils

-- | All supported source file extensions.
supportedExtensions :: [String]
supportedExtensions = [".cc", ".hpp", ".cpp", ".c", ".h"]

-- | Parse an argument list from the +file1 -file2 style into
--   two lists of file paths (included and ignored files).
parseFilter :: [String] -> Maybe ([FilePath], [FilePath])
parseFilter input = (filter' *** filter') <$> mapAndUnzipM go input
  where
  go ('+' : file) = Just (file, "")
  go ('-' : file) = Just ("", file)
  go _ = Nothing
  filter' = filter (not . null)

-- | Locate all source files recursively from the current directory.
findFiles :: IO [FilePath]
findFiles = explore "" "."
  where
  explore prefix dir = do
    contents <- (\\ [".", ".."]) <$> getDirectoryContents dir
    let withPrefix = map (prefix++) contents

    dirs <- filterM doesDirectoryExist withPrefix
    let sourceFiles = filter isValidSourceFile (withPrefix \\ dirs)
    nextDepth <- mapM exploreDir dirs
    return $ sourceFiles ++ concat nextDepth

  isValidSourceFile file = any (`isSuffixOf` file) supportedExtensions
  exploreDir dir = explore (dir ++ "/") dir

-- | Determine source code language based on file extensions.
determineLanguage :: [FilePath] -> Maybe KattisLanguage
determineLanguage = undefined

-- | Locate main class based on file paths and contents.
findMainClass :: ([FilePath], KattisLanguage) -> IO (Maybe FilePath)
findMainClass = undefined

-- | Determine content type of submission language.
-- TODO: Verify all non-c++ types.
languageContentType :: KattisLanguage -> B.ByteString
languageContentType LangCplusplus = "text/x-c++src"
languageContentType LangJava = "text/x-c++src"
languageContentType LangC = "text/x-c++src"

-- | Determine Kattis language string identifier.
-- TODO: Verify all non-c++ names.
languageKattisName :: KattisLanguage -> B.ByteString
languageKattisName LangCplusplus = "C++"
languageKattisName LangJava = "C"
languageKattisName LangC = "Java"
