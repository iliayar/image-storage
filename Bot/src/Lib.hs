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
import Storage

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
      Update {message = Just msg@(Message { photo = Just xs })} -> handleImageMessage msg
      Update {message = Just msg@(Message { document = Just x@(Document { doc_mime_type = Just (T.stripPrefix "image" -> Just _)})})} -> handleImageMessage msg
      Update {message = Just msg@(Message { text = Just _}) } -> handleMessage msg
      Update {callback_query = Just q} -> handleCallback q
      _ -> liftIO (putStrLn $ "Handle update failed. " ++ show update)
  liftIO $ hFlush stdout

handleCallback :: CallbackQuery -> Bot ()
handleCallback (CallbackQuery
                { cq_id = cqId
                , cq_from = (User {user_username = Just username})
                , cq_message = Just (Message {chat = (Chat {chat_id = chatId}), text = Just (T.lines -> (filename:_))})
                , cq_data = Just category
                }) = do
  liftIO $ putStrLn $ T.unpack username
  liftIO $ putStrLn $ T.unpack filename
  liftIO $ putStrLn $ T.unpack category
   

sendInlineMessage :: ChatId -> String -> String -> Bot ()
sendInlineMessage chatId username filename = do
  BotConfig{..} <- ask
  categories <- liftIO $ listCategories username
  let keyboard = ReplyInlineKeyboardMarkup $ map
        (\t -> [(InlineKeyboardButton (T.pack t) Nothing (Just $ T.pack $ t) Nothing  Nothing Nothing Nothing)]) categories
  liftIO $ sendMessage telegramToken
    (SendMessageRequest chatId (T.pack $ "`" ++ filename ++ "`\nIngore above") (Just Markdown) Nothing Nothing Nothing (Just keyboard)) manager
  liftIO $ return ()

handleImageMessage :: Message -> Bot ()
handleImageMessage msg = do
  BotConfig{..} <- ask
  let host = "https://bcbc6b25b466.ngrok.io" -- FIXME
      chatId = ChatId $ fromIntegral $ user_id $ fromJust $ from msg
      username = T.unpack $ fromJust $ user_username $ fromJust $ from msg
      processDownload (Just (url, filename)) =
        do
          -- Send message with keyboard here
          sendInlineMessage chatId username filename
          -- liftIO $ sendMessage telegramToken (sendMessageRequest chatId $ T.pack $ "Your image " ++ host ++ "/static/" ++ username ++ "/test/" ++ filename) manager
          liftIO $ downloadImage username "test" filename url
  case msg of
    Message { photo = Just xs } -> (fetchFilePath $ head $ reverse xs) >>= processDownload
    Message { document = Just x} -> (fetchFilePath x) >>= processDownload
  return ()
    

handleMessage :: Message -> Bot ()
handleMessage msg = do
  BotConfig{..} <- ask
  let chatId = ChatId $ fromIntegral $ user_id $ fromJust $ from msg
      username = T.unpack $ fromJust $ user_username $ fromJust $ from msg
      sendCategories = do
        categories <- liftIO $ listCategories username
        sendMessage telegramToken (sendMessageRequest chatId $ T.pack $ unlines categories) manager
  case fromJust $ text msg of
    (T.stripPrefix "/list" -> Just _) -> liftIO sendCategories >> return ()
    (T.stripPrefix "/new " -> Just (T.unpack -> c)) -> liftIO (createCategory username c) >> return ()
    (T.stripPrefix "/delete " -> Just (T.unpack -> c)) -> liftIO (deleteCategory username c) >> return ()
    _ -> liftIO $ putStrLn $ show msg
  return ()

class Fetchable a where
  getFileId :: a -> Text

instance Fetchable PhotoSize where
  getFileId PhotoSize { photo_file_id = id } = id

instance Fetchable Document where
  getFileId Document { doc_file_id = id } = id

fetchFilePath :: Fetchable a => a -> Bot (Maybe (String, String))
fetchFilePath f = do
  let id = getFileId f
  BotConfig{..} <- ask
  resp <- liftIO $ getFile telegramToken id manager
  liftIO $ case resp of
    Left err -> return Nothing
    Right (Response {result = File {file_path = Just path}}) -> return $
      Just ("http://api.telegram.org/file/" ++ (getStringToken telegramToken) ++ "/" ++ (T.unpack path), T.unpack $ getFileId f)
    Right (Response {result = File {file_path = Nothing}}) -> return Nothing
  where
    getStringToken (Token s) = T.unpack s
