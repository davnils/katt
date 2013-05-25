{-# Language OverloadedStrings, NoMonomorphismRestriction #-}

module Configuration (loadGlobalConfig, projectConfigExists, loadProjectConfig, saveProjectConfig) where

import Control.Error hiding (tryIO)
import Control.Monad
import qualified Control.Monad.State as S
import Control.Monad.Trans (liftIO, lift)
import qualified Data.ByteString.Char8 as B
import Data.ConfigFile
import Data.Monoid ((<>))
import qualified Network.URL as U
import System.Directory (getHomeDirectory)
import System.IO
import System.IO.Error (catchIOError)
import Utils

-- | Path to global configuration file, relative home directory.
globalConfigFile :: FilePath
globalConfigFile  = ".kattisrc"

projectConfigFile :: FilePath
projectConfigFile = B.unpack $ configDir <> "/" <> "config"

convertErrorDesc :: (Show a) => (a, String) -> B.ByteString
convertErrorDesc (errorData, str) = B.pack (show errorData) <> B.pack str

get' :: (Monad m, Get_C r) => ConfigParser -> SectionSpec -> String -> EitherT B.ByteString m r
get' conf section key = fmapLT
  (const . B.pack $ "Failed to parse " <> key <> " field")
  (get conf section key)

-- | Load global configuration file and parse the configuration state.
loadGlobalConfig :: IO (Either ErrorDesc ConfigState)
loadGlobalConfig = runEitherT $ do
  home <- tryIO getHomeDirectory
  let filePath = home <> "/" <> globalConfigFile

  fileHandle <- tryIOMsg ("Failed to open " <> B.pack filePath) $
    openFile filePath ReadMode
  conf <- fmapLT convertErrorDesc $ join . liftIO $ readhandle emptyCP fileHandle

  user' <- get' conf "user" "username"
  apiKey' <- get' conf "user" "token"

  loginURL <- get' conf "kattis" "loginurl"
  loginParsed <- extract "Failed to parse loginurl field" loginURL

  submitURL <- get' conf "kattis" "submissionurl"
  submitParsed <- extract "Failed to parse submissionurl field" submitURL

  host' <- parseHost loginParsed

  tryIOMsg "Failed to close configuration file handle" $ hClose fileHandle

  return $ ConfigState
    (B.pack user')
    (B.pack apiKey')
    (B.pack $ host')
    (B.pack $ U.url_path loginParsed)
    (B.pack $ U.url_path submitParsed)
    Nothing

  where
  extract msg = noteT msg . hoistMaybe . U.importURL
  parseHost (U.URL (U.Absolute host') _ _) = return $ U.host host'
  parseHost _ = left "Invalid URL format"

-- | Check if a project-specific configuration exists.
projectConfigExists :: IO Bool
projectConfigExists = catchIOError open . const $ return False
  where
  open = withFile projectConfigFile ReadMode . const $ return True

-- | Load a project-specific configuration file based on the current directory.
loadProjectConfig :: ConfigEnv IO ()
loadProjectConfig = do
  conf <- fmapLT convertErrorDesc $ join . liftIO $ readfile emptyCP projectConfigFile
  problem <- get' conf "problem" "problemname"
  lift . S.modify $ \s -> s { project = Just . ProblemName $ B.pack problem}

-- | Save a project-specific configuration file.
saveProjectConfig :: ConnEnv IO ()
saveProjectConfig = do
  project' <- lift . lift $ S.gets project
  state <- noteT "Tried to save an empty project configuration." . hoistMaybe $ project'
  problemName <- retrieveProblemName state
  serialized <- serialize problemName
  tryIOMsg (B.pack $
            "Failed to save project configuration file (path: "
            <> projectConfigFile <> ")") $
    writeFile projectConfigFile (to_string serialized)

  where
  serialize problemName = EitherT . return . fmapL convertErrorDesc $ do
    let conf = emptyCP
    conf <- add_section conf "problem"
    conf <- add_section conf "submissions"
    set conf "problem" "problemname" $ B.unpack problemName
