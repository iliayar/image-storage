module Storage
  ( listCategories
  , createCategory
  , deleteCategory
  , downloadImage
  )
  
where

import Network.HTTP.Client.Conduit.Download
import System.Directory
import System.FilePath
import System.IO.Error
import Control.Monad
import Control.Exception
import Data.Char

checkTraversal :: [FilePath] -> a -> IO a -> IO a
checkTraversal fs fail action =
  if all (all (isAlphaNum || (=='_'))) fs
  then action
  else return fail

listCategories :: FilePath -> IO [String]
listCategories user = checkTraversal [user] ["Incorrect username"] $ do
  res <- try $ listDirectory $ "static" </> user
  case res of
    Left err -> if isDoesNotExistError err then do
      res' <- try $ createDirectory $ "static" </> user :: IO (Either IOError ())
      case res' of
        Left err' -> putStrLn (show err') >> return ["Error happens"]
        Right _ -> listCategories user
      else putStrLn (show err) >> return ["Error happend"]
    Right entries -> filterM (doesDirectoryExist . (("static" </> user) </>)) entries
  
createCategory :: FilePath -> FilePath -> IO ()
createCategory user c = checkTraversal [user, c] () $ do
  res <- try $ createDirectory $ "static" </> user </> c :: IO (Either IOError ())
  case res of
    Left err -> putStrLn (show err) >> return ()
    Right _ -> return ()

deleteCategory :: FilePath -> FilePath -> IO ()
deleteCategory user c = checkTraversal [user, c] () $ do
  res <- try $ removeDirectoryRecursive $ "static" </> user </> c :: IO (Either IOError ())
  case res of
    Left err -> putStrLn (show err) >> return ()
    Right _ -> return ()
  
downloadImage :: FilePath -> FilePath -> FilePath -> String -> IO ()
downloadImage user c filename url = download url $ "static" </> user </> c </> filename
