{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics hiding (from)
import Control.Monad.Reader
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Data.Maybe
import Data.Monoid
import Web.Telegram.API.Bot
import System.Environment
import Data.Version (showVersion)
import qualified Paths_Bot as P
import System.IO

data Version = Version
  { version :: Text
  } deriving (Show, Generic)

instance ToJSON Version

type BotAPI = "version" :> Get '[JSON] Version
         :<|> "webhook"
              :> Capture "secret" Text
              :> ReqBody '[JSON] Update
              :> Post '[JSON] ()

botApi :: Proxy BotAPI
botApi = Proxy

startApp :: IO ()
startApp = do
  putStrLn "Bot is starting..."
  env <- getEnvironment
  manager' <- newManager tlsManagerSettings
  let telegramToken' = fromJust $ lookup "TELEGRAM_TOKEN" env
      config = BotConfig
        { telegramToken = Token $ T.pack $ "bot" <> telegramToken'
        , manager = manager'
        }
  putStrLn ("Bot token: " ++ telegramToken')
  hFlush stdout
  run 3001 $ app config

newtype Bot a = Bot
    { runBot :: ReaderT BotConfig Handler a
    } deriving ( Functor, Applicative, Monad, MonadIO, -- classes from base and transformers
                 MonadReader BotConfig, MonadError ServantErr) -- classes from mtl for

data BotConfig = BotConfig
  { telegramToken :: Token
  , manager :: Manager
  }

app :: BotConfig -> Application
app config = serve botApi $ initBotServer config

initBotServer :: BotConfig -> Server BotAPI
initBotServer config = enter (transform config) botServer
    where transform :: BotConfig -> Bot :~> ExceptT ServantErr IO
          transform config = Nat (flip runReaderT config . runBot)

botServer :: ServerT BotAPI Bot
botServer = returnVersion :<|> handleWebhook
    where version' = Version $ T.pack $ showVersion P.version
          returnVersion :: Bot Version
          returnVersion = return version'
          handleWebhook :: Text -> Update -> Bot ()
          handleWebhook secret update = do
              Token token <- asks telegramToken
              if EQ == compare secret token
                 then handleUpdate update
                 else throwError err403

handleUpdate :: Update -> Bot ()
handleUpdate update = do
    case update of
      Update {message = Just (Message { photo = Just xs })} -> handlePhotos xs
      Update {message = Just msg } -> handleMessage msg
      _ -> liftIO (putStrLn $ "Handle update failed. " ++ show update)
    liftIO $ hFlush stdout

handlePhotos :: [PhotoSize] -> Bot ()
handlePhotos = fetchPhotoPath . head . reverse

filterBiggest :: [PhotoSize] -> [PhotoSize]
filterBiggest [] = []
filterBiggest photos@((PhotoSize { photo_file_id = id}):xs) =
  (head $ fst s) : (filterBiggest $ snd s)
  where
    s = span (\PhotoSize {photo_file_id = x} -> x == id) photos

fetchPhotoPath :: PhotoSize -> Bot ()
fetchPhotoPath PhotoSize { photo_file_id = id } = do
  BotConfig{..} <- ask
  resp <- liftIO $ getFile telegramToken id manager
  liftIO $ case resp of
    Left err -> putStrLn $ show err
    Right (Response {result = File {file_path = Just path}}) -> putStrLn $ "http://api.telegram.org/file/" ++ (getStringToken telegramToken) ++ "/" ++ (T.unpack path)
    Right (Response {result = File {file_path = Nothing}}) -> putStrLn $ "No file path in response"
  where
    getStringToken (Token s) = T.unpack s

handleMessage :: Message -> Bot ()
handleMessage msg = liftIO . putStrLn $ case (text msg) of
                      Just x -> "New message " ++ (T.unpack x)
                      Nothing -> "Mes"
