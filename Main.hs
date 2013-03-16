{-# Language OverloadedStrings #-}

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.List (find)
import Network.Http.Client
import OpenSSL
import System.IO.Streams (readExactly)

host = "https://kth.kattis.scrool.se"
loginPage = "/login"
user = "davnils"

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

-- send some files to the upload page, retrieve submission id
submitSolution :: AuthEnv IO ()
submitSolution = return ()

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
  conn <- ask
  header <- liftIO . buildRequest $ do
    http POST loginPage
    defaultRequest
    setContentType "application/x-www-form-urlencoded"

  let formData = [("token", token), ("user", user), ("script", "true")] 
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
      Nothing -> liftIO $ putStrLn "Failed to authenticate"
      Just session -> do
        liftIO . putStrLn $ "authenticate returned temporary token: '" ++ B.unpack session ++ "'"
        runReaderT (submitSolution >> retrievePage "/judge_upload" >>= liftIO . putStrLn . B.unpack) session
