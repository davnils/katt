{-# Language OverloadedStrings #-}

module SourceHandler (parseFilter, findFiles, determineLanguage, findMainClass) where

import Control.Applicative
import qualified Data.ByteString.Char8 as B
import System.Directory
import System.IO
import Utils

-- | Parse an argument list in the +file1 -file2 style into
--   two lists of file paths (included and ignored files).
--   TODO: Complete.
parseFilter :: [String] -> Maybe ([FilePath], [FilePath])
parseFilter = join . go
  where
  go (('+' : file) : l) = Just (file, []) : go l
  go (('-' : file) : l) = Just ([], [file]) : go l
  go _ = Nothing

-- | Locate all source files recusively from the current directory.
findFiles :: IO [FilePath]
findFiles = filterstuff $ mapM_ findFiles <$> getDirectoryContents

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
