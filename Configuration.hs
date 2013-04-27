{-# Language OverloadedStrings, NoMonomorphismRestriction #-}

module Configuration where

import Control.Error hiding (tryIO)
import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.ConfigFile
import Data.Monoid ((<>))
import qualified Network.URL as U
import System.Directory (getHomeDirectory)
import System.IO
import Utils

-- | Path to global configuration file, relative home directory.
globalConfigFile :: FilePath
globalConfigFile  = ".kattisrc"

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
    (B.pack $ "https://" <> host')
    (B.pack $ U.url_path loginParsed)
    (B.pack $ U.url_path submitParsed)
    Nothing

  where
  convertErrorDesc (errorData, str) = B.pack (show errorData) <> B.pack str
  extract msg = noteT msg . hoistMaybe . U.importURL
  parseHost (U.URL (U.Absolute host') _ _) = return $ U.host host'
  parseHost _ = left "Invalid URL format"

  get' conf section key = fmapLT
    (const . B.pack $ "Failed to parse " <> key <> " field")
    (get conf section key)
