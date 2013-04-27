{-# Language OverloadedStrings #-}

module Upload where

import Blaze.ByteString.Builder (fromByteString)
import Control.Error hiding (tryIO)
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
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

submitSolution :: Submission -> AuthEnv IO SubmissionId
submitSolution (problem, files) = do
  let multiPartSeparator = "separator"

  header <- makeSignedRequest $ do
    http POST uploadPage
    defaultRequest
    setContentType $ B.append "multipart/form-data; boundary=" multiPartSeparator

  problemName <- noAuth $ retrieveProblemName problem

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
