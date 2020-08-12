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

import Storage

import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.List
import Data.Version (showVersion)
import qualified Data.Text as T

import Control.Monad.Reader
import Control.Monad.Except

import System.IO
import System.Environment
import System.FilePath (takeExtension, (<.>))

import GHC.Generics hiding (from)

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)

import Servant

import Web.Telegram.API.Bot

import qualified Paths_Bot as P

data Version = Version
  { version :: Text
  } deriving (Show, Generic)

instance ToJSON Version

type BotAPI = "version" :> Get '[JSON] Version
         :<|> "webhook"
              :> Capture "secret" Text
              :> ReqBody '[JSON] Update
              :> Post '[JSON] ()

class Fetchable a where
  getFileId :: a -> Text

instance Fetchable PhotoSize where
  getFileId PhotoSize { photo_file_id = id } = id

instance Fetchable Document where
  getFileId Document { doc_file_id = id } = id

newtype Bot a = Bot
    { runBot :: ReaderT BotConfig Handler a
    } deriving ( Functor, Applicative, Monad, MonadIO,
                 MonadReader BotConfig, MonadError ServantErr)

data BotConfig = BotConfig
  { telegramToken :: Token
  , manager :: Manager
  , host :: Text
  , botName :: Text
  }

botApi :: Proxy BotAPI
botApi = Proxy

startApp :: IO ()
startApp = do
  putStrLn "Bot is starting..."
  env <- getEnvironment
  manager' <- newManager tlsManagerSettings
  let telegramToken' = fromJust $ lookup "TELEGRAM_TOKEN" env
      host' = fromJust $ lookup "WEB_HOST" env
      botName' = fromJust $ lookup "TELEGRAM_BOT_NAME" env
      config = BotConfig
        { telegramToken = Token $ T.pack $ "bot" <> telegramToken'
        , manager = manager'
        , host = T.pack host'
        , botName = T.pack botName'
        }
  setWebhook (telegramToken config) (Just $ "https://" <> (host config) <> "/bot/webhook/bot" <> (T.pack telegramToken')) manager'
  hFlush stdout
  run 3001 $ app config

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
      Update {message = Just msg@(Message { document = Just x@(Document { doc_mime_type = Just (T.stripPrefix "video" -> Just _)})})} -> handleImageMessage msg
      Update {message = Just msg@(Message { text = Just _}) } -> handleMessage msg
      Update {callback_query = Just q} -> handleCallback q
      _ -> liftIO (putStrLn $ "Handle update failed. " ++ show update)
  liftIO $ hFlush stdout

handleCallback :: CallbackQuery -> Bot ()
handleCallback (CallbackQuery
                { cq_id = cqId
                , cq_from = (User {user_username = Just (T.unpack -> username)})
                , cq_message = Just (Message { chat = (Chat {chat_id = chatId})
                                             , text = Just (T.lines -> (filename:_))
                                             , reply_to_message = Just replMsg
                                             })
                , cq_data = Just (T.unpack -> category)
                }) = do
  BotConfig{..} <- ask
  let answerQuery s = answerCallbackQuery telegramToken (AnswerCallbackQueryRequest cqId (Just s) Nothing Nothing Nothing) manager
      processDownload (Just (url, filename)) = liftIO $ do
        downloadImage username category filename url
        answerQuery "File saved"
        sendMessage telegramToken (sendMessageRequest (ChatId $ fromIntegral chatId) $
                                   T.pack $ "Your image " ++ intercalate "/" [T.unpack host, "static", username, category, filename]) manager
        return ()
  case replMsg of
    Message { photo = Just xs } -> (fetchFilePath $ head $ reverse xs) >>= processDownload
    Message { document = Just x } -> (fetchFilePath x) >>= processDownload
    Message { text = Just (T.stripPrefix "/delete" -> Just _)} -> liftIO $ deleteCategory username category >> answerQuery "Category deleted" >> return ()
  return ()
   

sendInlineForceReplyMessage :: Text -> Int -> ChatId -> Bot ()
sendInlineForceReplyMessage text messageId chatId = do
  BotConfig{..} <- ask
  liftIO $ sendMessage telegramToken
    (SendMessageRequest chatId text Nothing Nothing Nothing (Just messageId) (Just $ ForceReply True Nothing)) manager
  return ()
  

sendInlineMessage :: Text -> Int -> ChatId -> String -> Bot ()
sendInlineMessage text messageId chatId username = do
  BotConfig{..} <- ask
  categories <- liftIO $ listCategories username
  let keyboard = ReplyInlineKeyboardMarkup $ map
        (\t -> [(InlineKeyboardButton (T.pack t) Nothing (Just $ T.pack $ t) Nothing  Nothing Nothing Nothing)]) categories
  liftIO $ sendMessage telegramToken
    (SendMessageRequest chatId text Nothing Nothing Nothing (Just messageId) (Just keyboard)) manager
  return ()

handleImageMessage :: Message -> Bot ()
handleImageMessage msg@(Message {message_id = messageId}) = do
  BotConfig{..} <- ask
  let chatId = ChatId $ fromIntegral $ user_id $ fromJust $ from msg
      username = T.unpack $ fromJust $ user_username $ fromJust $ from msg
  sendInlineMessage "Choose category to save image in" messageId chatId username
  return ()
    

handleMessage :: Message -> Bot ()
handleMessage msg@(Message {message_id = messageId}) = do
  BotConfig{..} <- ask
  let chatId = ChatId $ fromIntegral $ user_id $ fromJust $ from msg
      username = T.unpack $ fromJust $ user_username $ fromJust $ from msg
      sendCategories = do
        categories <- liftIO $ listCategories username
        sendMessage telegramToken (sendMessageRequest chatId $ T.pack $ unlines categories) manager
  case msg of
    Message { text = Just (T.unpack -> c)
            , reply_to_message = Just (Message { text = Just (T.stripPrefix "Choose name" -> Just _)
                                               , from = Just (User { user_first_name = botName})
                                               })} -> liftIO (createCategory username c) >> return ()
    _ -> case fromJust $ text msg of
           (T.stripPrefix "/list" -> Just _) -> liftIO sendCategories >> return ()
           (T.stripPrefix "/new" -> Just _) -> sendInlineForceReplyMessage "Choose name for new category" messageId chatId
           (T.stripPrefix "/delete" -> Just _) -> sendInlineMessage "Choose category to delete" messageId chatId username
           _ -> liftIO $ putStrLn $ show msg
  return ()

fetchFilePath :: Fetchable a => a -> Bot (Maybe (String, String))
fetchFilePath f = do
  let id = getFileId f
  BotConfig{..} <- ask
  resp <- liftIO $ getFile telegramToken id manager
  liftIO $ case resp of
    Left err -> return Nothing
    Right (Response {result = File {file_path = Just path}}) -> return $
      Just ("http://api.telegram.org/file/" ++ (getStringToken telegramToken) ++ "/" ++ (T.unpack path), (T.unpack id) <.> (takeExtension $ T.unpack path))
    Right (Response {result = File {file_path = Nothing}}) -> return Nothing
  where
    getStringToken (Token s) = T.unpack s
