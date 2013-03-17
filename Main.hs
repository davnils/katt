{-# Language OverloadedStrings #-}

import Blaze.ByteString.Builder (fromByteString)
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.List (find)
import Network.Http.Client
import OpenSSL
import System.IO.Streams (readExactly, write)

-- host = "http://78.47.152.189"
host = "https://kth.kattis.scrool.se"
user = "davnils"

loginPage = "/login"
uploadPage = "/judge_upload"

loginSuccess = "Login successful"

-- TODO: integrate error layer in ConnEnv
--       common errors should be accessible through monad transformer
--       but expections might still occur and shoulded generate plenty of debug data

type ConnEnv m = ReaderT Connection m
type AuthEnv m = ReaderT B.ByteString (ConnEnv m)

makeSignedRequest :: RequestBuilder () -> AuthEnv IO Request
makeSignedRequest req = do
  key <- liftM (setHeader "Cookie") ask
  liftIO . buildRequest $ req >> key

defaultRequest :: RequestBuilder ()
defaultRequest = do
  setHeader "User-Agent" "Sofie"
  setHeader "Connection" "keep-alive"

data MultiPartField
  = Option [B.ByteString] B.ByteString
  | File FilePath

crlf = "\r\n"

-- buildChunk :: MultiPartField -> IO B.ByteString
buildChunk (File path) = do
  file <- B.readFile path
  return $ B.intercalate crlf [headerLine, "Content-Type: text/x-c++src", "", file]
  where
    headerLine = B.intercalate "; " ["Content-Disposition: form-data", "name=\"sub_file[]\"", B.concat ["filename=\"", (B.pack path), "\""]]

buildChunk (Option fields payload) = return $ B.intercalate crlf [headerLine, "", payload]
  where
    headerLine = B.intercalate "; " fieldList
    fieldList = "Content-Disposition: form-data" : fields

-- TODO: Fix issue with host being replaced with "<default>"

-- send some files to the upload page, retrieve submission id
submitSolution :: AuthEnv IO ()
submitSolution = do
  -- include the following field-value mappings:
  -- submit = true
  -- submit_ctr = 2
  -- language = <integer> (default to 1)
  -- mainclass = ""
  -- problem = "hello"
  -- tag = ""
  -- script = "true"
  --
  -- sub_file[] = ["file1", "file2", ...] with additional filename="filename"

  let multiPartSeparator = "---------------------------qeI7b0RwXcMbHVuWonwOpXbhvMB_"

  header <- makeSignedRequest $ do
    http POST uploadPage
    defaultRequest
    setContentType $ B.append "multipart/form-data; boundary=" multiPartSeparator

  let postFields = [Option ["name=\"submit\""] "true", Option ["name=\"submit_ctr\""] "2", Option ["name=\"language\""] "1", Option ["name=\"mainclass\""] "", Option ["name=\"problem\""] "hello", Option ["name=\"tag\""] "", Option ["name=\"script\""] "true", File "example.cc"]
  -- let postFields = [File "file_name_0", File "file_name_1"]

  liftIO $ putStrLn "executing sendRequest"
  liftIO $ putStrLn $ "Using header: " ++ show header

  conn <- lift $ ask
  liftIO $ sendRequest conn header (\o -> do
    mapM_ (\part -> do
      serialized <- buildChunk part
      putStrLn $ "writing: " ++ B.unpack serialized
      write (Just . fromByteString $ B.concat ["--", multiPartSeparator, crlf, serialized]) o)
      postFields

    write (Just $ fromByteString $ B.concat ["--", multiPartSeparator, "--", crlf]) o
    )

  liftIO $ putStrLn "sendRequest completed"
  liftIO $ receiveResponse conn debugHandler

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

  -- return $ Just "thisisatemptoken"

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
      Nothing -> liftIO $ putStrLn "Failed to authenticate"
      Just session -> do
        liftIO . putStrLn $ "authenticate returned temporary token: '" ++ B.unpack session ++ "'"
        runReaderT (submitSolution) session
