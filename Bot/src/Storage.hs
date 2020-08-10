module Storage
  ( listCategories
  )
  
where

import System.Directory
import System.FilePath
import System.IO.Error
import Control.Monad
import Control.Exception
import Data.Char


listCategories :: FilePath -> IO [String]
listCategories user = do
  res <- try $ listDirectory $ "static" </> user
  case res of
    Left err -> if isDoesNotExistError err then do
      res' <- try $ createDirectory $ "static" </> user :: IO (Either IOError ())
      case res' of
        Left err' -> putStrLn (show err') >> return ["Error happens"]
        Right _ -> listCategories user
      else putStrLn (show err) >> return ["Error happend"]
    Right entries -> filterM (doesDirectoryExist . (("static" </> user) </>)) entries
  
