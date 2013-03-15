{-# Language OverloadedStrings #-}

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
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

signRequest :: AuthEnv RequestBuilder ()
signRequest = ask >>= \session -> lift . lift $ setHeader "Cookie" session

defaultRequest :: RequestBuilder ()
defaultRequest = setHeader "User-Agent" "Sofie"

-- send some files to the upload page, retrieve submission id
submitSolution :: AuthEnv IO ()
submitSolution = undefined

authenticate :: ConnEnv IO (Maybe B.ByteString)
authenticate = do
  conn <- ask
  header <- lift $ buildRequest conn $ do
    http POST loginPage
    defaultRequest
    setContentType "application/x-www-form-urlencoded"

  let formData = [("token", token), ("user", user), ("script", "true")] 
  liftIO $ sendRequest conn header $ encodedFormBody formData

  -- parse response header and verify that body content matches the magic string
  result <- liftIO $ receiveResponse conn (\headers stream -> do
    response <- readExactly (B.length loginSuccess) stream
    putStrLn $ "Received response: '" ++ B.unpack response ++ "' and headers: " ++ show headers
    return (headers, response == loginSuccess))

  case result of
    (_, False) -> return Nothing
    (headers, _) -> return $ getHeader headers "Set-Cookie"

main :: IO ()
main = withOpenSSL $ withConnection (establishConnection host) $ runReaderT work
  where
  work = do
    status <- authenticate
    case status of
      Nothing -> liftIO $ putStrLn "Failed to authenticate"
      Just session -> do
        liftIO $ putStrLn $ "authenticate returned cookie: '" ++ B.unpack session ++ "'"
        runReaderT submitSolution session
