{-# Language OverloadedStrings #-}

import Blaze.ByteString.Builder (fromByteString)
import Control.Applicative
import Control.Error
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import Data.List (find)
import Data.Monoid ((<>))
import Text.Regex.Posix
import Network.Http.Client
import OpenSSL
import System.IO.Streams (readExactly, write)

host, user, token, loginPage, uploadPage, loginSuccess, crlf :: B.ByteString

host = "https://kth.kattis.scrool.se"
user = "davnils"
token = ""

loginPage = "/login"
uploadPage = "/judge_upload"

loginSuccess = "Login successful"

-- TODO: integrate error layer in ConnEnv
--       common errors should be accessible through monad transformer
--       but expections might still occur and shoulded generate plenty of debug data
-- use the errors module
-- EitherT B.ByteString m a
-- how should it be integrated into the monad transformer stack?
-- should be able to operate with error handling over ConnEnv and AuthEnv

-- should EitherT be integrated into  ConnEnv/AuthEnv aliases?
-- probably, if it doesn't interfere.
-- should EitherT occur twice in the stack alias? no!

type ConnEnvInternal m = ReaderT Connection m
type ConnEnv m e = EitherT e (ConnEnvInternal m)
type AuthEnv m e = EitherT e (ReaderT B.ByteString (ConnEnvInternal m))
type Submission = (B.ByteString, [FilePath])
type SubmissionId = Integer

noauth :: Monad m => ConnEnv m e a -> AuthEnv m e a
noauth = EitherT . lift . runEitherT

makeSignedRequest :: RequestBuilder () -> AuthEnv IO () Request
makeSignedRequest req = do
  key <- return $ liftM (setHeader "Cookie") ask
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

submitSolution :: Submission -> AuthEnv IO () SubmissionId
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

  conn <- lift ask
  liftIO $ sendRequest conn header (\o -> do
    mapM_ (\part -> do
      serialized <- buildChunk part
      write (Just . fromByteString $ B.concat ["--", multiPartSeparator, crlf, serialized]) o)
      postFields

    write (Just . fromByteString $ B.concat ["--", multiPartSeparator, "--", crlf]) o
    )

  reply <- liftIO $ receiveResponse conn concatHandler
  return . read . B.unpack $ reply =~ ("[0-9]+" :: B.ByteString)

retrievePage :: B.ByteString -> AuthEnv IO B.ByteString
retrievePage page = do
  header <- makeSignedRequest $ do
    http GET page
    defaultRequest

  conn <- lift ask
  liftIO $ sendRequest conn header emptyBody
  liftIO $ receiveResponse conn concatHandler

authenticate :: ConnEnv IO (Maybe B.ByteString)
authenticate = do
  header <- liftIO . buildRequest $ do
    http POST loginPage
    defaultRequest
    setContentType "application/x-www-form-urlencoded"

  let formData = [("token", token), ("user", user), ("script", "true")] 
  conn <- ask
  liftIO $ sendRequest conn header $ encodedFormBody formData

  -- TODO: Catch exception upon failed read
  result <- liftIO $ receiveResponse conn (\headers stream -> do
    response <- readExactly (B.length loginSuccess) stream
    return (headers, response == loginSuccess))

  return $ case result of
    (_, False) -> Nothing
    (headers, _) ->  
      (B.words <$> getHeader headers "Set-Cookie") >>=
      find ("PHPSESSID" `B.isPrefixOf`) >>=
      return . B.takeWhile (/= ';')

main :: IO ()
main = withOpenSSL . withConnection (establishConnection host) $ runReaderT go
  where
  go = do
    status <- authenticate
    case status of
      Nothing -> liftIO $ putStrLn "[-] failed to authenticate"
      Just session -> do
        liftIO . putStrLn $ "[*] temporary token: '" ++ B.unpack session ++ "'"
        runReaderT (submitSolution ("hello", ["example.cc"]) >>= liftIO . putStrLn . show) session
