{-# Language OverloadedStrings, ScopedTypeVariables #-}

import Blaze.ByteString.Builder (fromByteString)
import Control.Applicative
import Control.Error hiding (tryIO)
import Control.Exception (SomeException, try)
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import Data.List (find)
import Data.Monoid ((<>))
import Text.Regex.Posix
import Network.Http.Client
import OpenSSL
import System.Exit (exitFailure)
import System.IO (stderr)
import System.IO.Streams (readExactly, write)

host, user, token, loginPage, uploadPage, loginSuccess, crlf :: B.ByteString

host = "https://kth.kattis.scrool.se"
user = "davnils"
token = ""

loginPage = "/login"
uploadPage = "/judge_upload"

loginSuccess = "Login successful"

type ConnEnvInternal m = ReaderT Connection m
type ConnEnv m = EitherT ErrorDesc (ConnEnvInternal m)
type AuthEnv m = EitherT ErrorDesc (ReaderT B.ByteString (ConnEnvInternal m))
type Submission = (B.ByteString, [FilePath])
type SubmissionId = Integer
type ErrorDesc = B.ByteString

noauth :: (Monad m, MonadTrans t) => EitherT e m a -> EitherT e (t m) a
noauth = EitherT . lift . runEitherT

tryIO :: MonadIO m => IO a -> EitherT ErrorDesc m a
tryIO = EitherT . liftIO . liftM (fmapL (B.pack . show)) . 
  (try :: (IO a -> IO (Either SomeException a)))

terminateOnFailure :: MonadIO m => ErrorDesc -> EitherT ErrorDesc m a -> m a
terminateOnFailure msg state = do
  res <- runEitherT state
  liftIO $ case res of
    Left err -> do
      B.hPutStrLn stderr $ msg <> ", error: " <> err
      exitFailure
    Right succ -> return succ

makeSignedRequest :: RequestBuilder () -> AuthEnv IO Request
makeSignedRequest req = do
  key <- lift $ liftM (setHeader "Cookie") ask
  liftIO . buildRequest $ req >> key

defaultRequest :: RequestBuilder ()
defaultRequest = do
  setHeader "User-Agent" "Sofie"
  setHeader "Connection" "keep-alive"

data MultiPartField
  = Option [B.ByteString] B.ByteString
  | File FilePath

crlf = "\r\n"

buildChunk :: MultiPartField -> IO B.ByteString
buildChunk (File path) = do
  file <- B.readFile path
  return $ B.intercalate crlf [headerLine, "Content-Type: text/x-c++src", "", file, ""]
  where
    headerLine = B.intercalate "; " ["Content-Disposition: form-data", "name=\"sub_file[]\"",
                                     B.concat ["filename=\"", (B.pack path), "\""]]

buildChunk (Option fields payload) = return $ B.intercalate crlf [headerLine, "", payload, ""]
  where
    headerLine = B.intercalate "; " fieldList
    fieldList = "Content-Disposition: form-data" : fields

submitSolution :: Submission -> AuthEnv IO SubmissionId
submitSolution (problemId, files) = do
  let multiPartSeparator = "separator"

  header <- makeSignedRequest $ do
    http POST uploadPage
    defaultRequest
    setContentType $ B.append "multipart/form-data; boundary=" multiPartSeparator

  let postFields = [Option ["name=\"submit\""] "true"]
                <> [Option ["name=\"submit_ctr\""] "2"]
                <> [Option ["name=\"language\""] "C++"]
                <> [Option ["name=\"mainclass\""] ""]
                <> [Option ["name=\"problem\""] problemId]
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

retrievePage :: B.ByteString -> AuthEnv IO B.ByteString
retrievePage page = do
  header <- makeSignedRequest $ do
    http GET page
    defaultRequest

  conn <- lift . lift $ ask
  tryIO $ sendRequest conn header emptyBody
  tryIO $ receiveResponse conn concatHandler

authenticate :: ConnEnv IO B.ByteString
authenticate = do
  header <- tryIO . buildRequest $ do
    http POST loginPage
    defaultRequest
    setContentType "application/x-www-form-urlencoded"

  let formData = [("token", token), ("user", user), ("script", "true")] 
  conn <- ask
  tryIO . sendRequest conn header $ encodedFormBody formData

  (headers, response) <- tryIO $ receiveResponse conn (\headers stream -> do
    response <- readExactly (B.length loginSuccess) stream
    return (headers, response))

  tryAssert ("Login failure. Server returned: '" <> response <> "'") (response == loginSuccess)
    
  noteT "Failed to parse login cookie" . hoistMaybe $ do
    cookies <- B.words <$> getHeader headers "Set-Cookie"
    B.takeWhile (/= ';') <$> find ("PHPSESSID" `B.isPrefixOf`) cookies

main :: IO ()
main = withOpenSSL . withConnection (establishConnection host) $ runReaderT go
  where
  go = do
    session <- terminateOnFailure "Authentication failed" authenticate
    liftIO . putStrLn $ "[*] temporary token: '" ++ B.unpack session ++ "'"
    runReaderT (terminateOnFailure "Submission failed" (submitSolution ("hello", ["example.cc"])) >>= liftIO . putStrLn . show) session
