{-# Language OverloadedStrings, ScopedTypeVariables #-}

module Upload (makeSubmission, submitSolution) where

import Control.Applicative ((<$>))
import Blaze.ByteString.Builder (fromByteString)
import qualified Configuration as C
import Control.Error hiding (tryIO)
import Control.Monad.Reader
import qualified Control.Monad.State as S
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Network.Http.Client
import System.IO.Streams (write)
import Text.Regex.Posix
import Utils

uploadPage :: B.ByteString
uploadPage = "/judge_upload"

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

makeSubmission :: ConnEnv IO ()
makeSubmission = do
  token <- authenticate 

  exists <- liftIO C.projectConfigExists
  tryAssert "No project configuration could be found."
    exists

  unWrapTrans C.loadProjectConfig
  problem <- lift (fromJust <$> S.gets project)

  -- TODO: locate source and header files

  submission <- EitherT $ runReaderT (runEitherT $ submitSolution (problem, ["main.cc"])) token
  liftIO . putStrLn $ "Made submission: " <> show submission

submitSolution :: Submission -> AuthEnv IO SubmissionId
submitSolution (problem, files) = do
  let multiPartSeparator = "separator"

  header <- makeSignedRequest $ do
    http POST uploadPage
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

  conn <- lift . lift $ ask
  tryIO $ sendRequest conn header (\o -> do
    mapM_ (\part -> do
        serialized <- buildChunk part
        write (Just . fromByteString $ B.concat ["--", multiPartSeparator, crlf, serialized]) o)
      postFields

    write (Just . fromByteString $ B.concat ["--", multiPartSeparator, "--", crlf]) o
    )

  reply <- tryIO $ receiveResponse conn concatHandler
  tryRead "Failed to parse submission id from server" . B.unpack $ reply =~ ("[0-9]+" :: B.ByteString)
