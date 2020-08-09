{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
module Hello where

import Yesod.Core

data Hello = Hello

mkYesod "Hello" [parseRoutes|
  /      HomeR  GET
  /about AboutR GET
|]

instance Yesod Hello where
        approot = ApprootStatic ""

getHomeR = defaultLayout [whamlet|
  Hello World!
  <br>
  <a href=@{AboutR}>About Us.
|]

getAboutR = defaultLayout [whamlet|
  Enough about us!
  <br>
  <a href=@{HomeR}>Back Home.
|]

