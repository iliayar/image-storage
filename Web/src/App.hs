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
/storage/#Text/#Text StorageR GET
|]

instance Yesod App where
  approot = ApprootStatic ""

renderMedia user category (Image file) =
  [whamlet|
          <img src=#{mediaLink user category file}>
          |]
renderMedia user category (Video file) =
  [whamlet|
          <video autoplay loop muted playsinline src=#{mediaLink user category file}>
          |]
renderMedia user category (Unknown file) =
  [whamlet|
          <a href=#{mediaLink user category file}>#{file}
          |]

getStorageR :: Text -> Text -> Handler Html
getStorageR user category = defaultLayout $ do
  media <- liftIO $ listMedia user category
  setTitle $ "Storage"
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