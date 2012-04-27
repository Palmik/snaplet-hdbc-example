{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Foo where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Database.HDBC as RDB
import           Data.Maybe
import qualified Data.Text as T (pack)
import           Data.Text (Text)
import qualified Data.Map as M
import           Data.Map ((!))
------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Heist
import           Text.Digestive
import           Text.Templating.Heist
------------------------------------------------------------------------------
import           Application

data User = User { name :: String
                 , age :: Int
                 } deriving Show


------------------------------------------------------------------------------
-- | CONTROLLER
userForm :: Monad m => Form Text m User
userForm = User <$>
           "name" .: string Nothing <*>
           "age"  .: stringRead "Not a number" Nothing

userSingleHandler :: ByteString -> User -> Handler App App ()
userSingleHandler tpl u = ifTop $ heistLocal (bindSplices [("user-single", userSingleSplice u)]) $ render tpl

userListHandler :: ByteString -> [User] -> Handler App App ()
userListHandler tpl us = ifTop $ heistLocal (bindSplices [("user-list", userListSplice us)]) $ render tpl

userListSplice :: (Monad m) => [User] -> Splice m
userListSplice = mapSplices userSingleSplice

userSingleSplice :: (Monad m) => User -> Splice m
userSingleSplice u = runChildrenWith $ map apply
                         [ ("user-name", userNameSplice)
                         , ("user-age",  userAgeSplice)
                         ]
    where apply (s, f) = (s, f u)

userNameSplice :: (Monad m) => User -> Splice m
userNameSplice u = textSplice . T.pack $ name u

userAgeSplice :: (Monad m) => User -> Splice m
userAgeSplice u = textSplice . T.pack . show $ age u
 
------------------------------------------------------------------------------
-- | MODEL                 
prepareDB :: RDB.IConnection c => c -> IO ()
prepareDB conn = do
    tb <- RDB.getTables conn
    unless ("users" `elem` tb) $ do
        RDB.run conn ("CREATE TABLE users\
                     \( uid   INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT \
                     \, uname TEXT NOT NULL \
                     \, uage  INTEGER NOT NULL)") []
        return ()
    RDB.commit conn

insertUser :: HasHdbc m c s => User -> m Integer
insertUser User{..} = do
    query' ("INSERT into users(uname, uage) VALUES(?, ?)")
           [toSql name, toSql age]
    [rs] <- query "SELECT LAST_INSERT_ROWID() AS uid" []
    return $! fromSql (rs ! "uid")

retrieveAllUsers :: HasHdbc m c s => m [User]
retrieveAllUsers = do
    res <- query ("SELECT * FROM users") []
    return $! mapMaybe mapToUser res

retrieveUser :: HasHdbc m c s => Integer -> m (Maybe User)
retrieveUser uid = do
    res <- query ("SELECT * FROM users WHERE uid = ?") [toSql uid]
    case res of
         [x] -> return $! mapToUser x
         _   -> return Nothing

mapToUser :: M.Map String SqlValue -> Maybe User
mapToUser rs = User <$>
               (fromSql <$> M.lookup "uname" rs) <*>
               (fromSql <$> M.lookup "uage" rs)
