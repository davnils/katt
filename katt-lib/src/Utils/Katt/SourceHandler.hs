{-# Language OverloadedStrings #-}

--------------------------------------------------------------------
-- |
-- Module : Utils.Katt.SourceHandler
--
-- Provides searching of source code files and language identification.
--
-- Language identification is required in order to detect any
-- inconsistencies (e.g. combining Java and C), and to tag submissions.
--
-- Java also requires identifying which file provides the main method.

module Utils.Katt.SourceHandler
(parseFilter, findFiles, determineLanguage, findMainClass, languageKattisName, languageContentType)
where

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.List
import qualified Data.Set as Set
import System.Directory
import System.FilePath (takeBaseName, takeExtension)
import Text.Parsec
import Text.Parsec.ByteString
import Utils.Katt.Utils

-- | All supported source file extensions, per language.
supported :: KattisLanguage -> Set.Set FilePath
supported LangCplusplus = Set.fromList [".cc", ".cpp", ".hpp", ".h"]
supported LangC         = Set.fromList [".c", ".h"]
supported LangJava      = Set.fromList [".java"]

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

  isValidSourceFile file = any (`isSuffixOf` file)
    (Set.toList . Set.unions $ map supported [LangCplusplus, LangC, LangJava])
  exploreDir dir = explore (dir ++ "/") dir

-- | Determine source code language by studying file extensions.
--   There is an implicit priority ordering, since C is a subset of C++.
determineLanguage :: [FilePath] -> Maybe KattisLanguage
determineLanguage files
  | is LangC = Just LangC
  | is LangCplusplus = Just LangCplusplus
  | is LangJava = Just LangJava
  | otherwise = Nothing
  where
  fileSet = Set.fromList $ map takeExtension files
  is lang = fileSet `Set.isSubsetOf` supported lang

-- | Locate main class based on source file contents.
--   C++ and C solutions do not need to be specified, returns an empty string.
--
--   In the Java case all souce code files are parsed.
--   All occurences of a /main/ method defined with /public static void/ are located.
--
--   Will return 'Data.Maybe.Nothing' if result is ambiguous.
findMainClass :: ([FilePath], KattisLanguage) -> IO (Maybe FilePath)
findMainClass ([], _)            = return Nothing
findMainClass (_, LangCplusplus) = return $ Just ""
findMainClass (_, LangC)         = return $ Just ""
findMainClass (files, LangJava)  = survey <$> filterM containsMain files
  where
  containsMain file = do
    parseResult <- parseFromFile mainParser file
    case parseResult of
      Right _ -> return True
      Left _ -> return False

  mainParser = manyTill
    (lineComment <|> blockComment <|> stringData <|> void anyChar)
    mainFunc
  blockComment = void $ string "/*" >> manyTill anyChar (try $ string "*/")
  lineComment = void . try $ string "//" >> manyTill anyChar newline
  stringData = void $ char '"' >> manyTill anyChar (char '"')
  mainFunc = try $ mapM_ keyWord ["public", "static", "void", "main"]
  keyWord str = void $ string str >> spaces

  survey [singleton] = Just $ takeBaseName singleton
  survey _ = Nothing

-- | Determine content type of submission language.
languageContentType :: KattisLanguage -> B.ByteString
languageContentType LangCplusplus = "text/x-c++src"
languageContentType LangJava = "text/x-c++src"
languageContentType LangC = "text/x-c++src"

-- | Determine Kattis language string identifier.
languageKattisName :: KattisLanguage -> B.ByteString
languageKattisName LangCplusplus = "C++"
languageKattisName LangJava = "Java"
languageKattisName LangC = "C"
