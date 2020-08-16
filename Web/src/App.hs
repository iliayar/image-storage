{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE QuasiQuotes           #-}

module App where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.List
import           Control.Monad
import           System.Directory
import           System.FilePath ((</>), takeExtension)
import           Yesod.Core


data Media = Image Text | Video Text | Unknown Text
mediaLink :: Text -> Text -> Text -> Text
mediaLink user category file = "/static/" <> user <> "/" <> category <> "/" <> file

data App = App
mkYesod "App" [parseRoutes|
/storage/#Text UserStorageR GET
/storage/#Text/#Text StorageR GET
|]

instance Yesod App where
  approot = ApprootStatic ""

renderMedia user category (Image file) =
  [whamlet|
          $with url <- mediaLink user category file
            <a href=#{url}><img class="media" src=#{url}>
          |]
renderMedia user category (Video file) =
  [whamlet|
          $with url <- mediaLink user category file
            <a href=#{url}><video class="media" autoplay loop muted playsinline src=#{url}>
          |]
renderMedia user category (Unknown file) =
  [whamlet|
          <a href=#{mediaLink user category file}>#{file}
          |]

mediaCssWidget = 
  toWidget [lucius|
                  .media {
                      max-width: 20%;
                      width: 100%;
                      display: inline;
                  }
                  |]

getUserStorageR :: Text -> Handler Html
getUserStorageR user = defaultLayout $ do
  media <- liftIO $ listUserMedia user
  setTitle $ "User storage"
  mediaCssWidget
  toWidget [hamlet|
                  <h1>#{user}
                  |]
  toWidget [whamlet|
                   $forall (category, m) <- media
                     ^{renderMedia user category m}|]
  
getStorageR :: Text -> Text -> Handler Html
getStorageR user category = defaultLayout $ do
  media <- liftIO $ listMedia user category
  setTitle $ "Storage"
  mediaCssWidget
  toWidget [hamlet|
                   <h1>#{user}
                   <h2>#{category}|]
  toWidget [whamlet|
                   $forall m <- media
                     ^{renderMedia user category m}|]

toMedia :: FilePath -> Media
toMedia file = (matchExtension $ takeExtension file) $ T.pack file
  where
    matchExtension ".png" = Image
    matchExtension ".jpg" = Image
    matchExtension ".mp4" = Video
    matchExtension _ = Unknown

    
listMedia :: Text -> Text -> IO [Media]
listMedia user category = do
  media <- listDirectory $ "static" </>  (T.unpack user) </> (T.unpack category)
  mapM (return . toMedia) media

listUserMedia :: Text -> IO [(Text, Media)]
listUserMedia user = do
  cs <- listDirectory $ "static" </> (T.unpack user)
  fmap concat $ mapM (\ c -> listMedia user c >>= mapM (\ m -> return (c, m))) $ map T.pack cs
  
