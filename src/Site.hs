{-# LANGUAGE OverloadedStrings #-}

module Site
( app
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (unpack)
import           Database.HDBC.Sqlite3
import           Text.Digestive.Snap hiding (method)
import           Text.Digestive.Heist
------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application
import           Foo


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
indexHandler :: Handler App App ()
indexHandler = do
    (view, result) <- runForm "form" userForm
    case result of
        Just x  -> insertUser x >> redirect "/"
        Nothing -> heistLocal (bindDigestiveSplices view) $ render "index"

usersHandler :: Handler App App ()
usersHandler = do
    us <- retrieveAllUsers
    userListHandler "users" us

userHandler :: Handler App App ()
userHandler = do
    mn <- readParam "uid"
    mu <- maybe (return Nothing) retrieveUser mn
    maybe pass (userSingleHandler "user") mu


readParam :: (Read a, MonadSnap m) => ByteString -> m (Maybe a)
readParam n = maybe Nothing (\bs -> readMaybe (BS.unpack bs)) <$> getParam n
    
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                   [(x, "")] -> Just x
                   _         -> Nothing
    
------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/",            indexHandler)
         , ("/user/:uid",   userHandler)
         , ("/users",       usersHandler)
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    let conn = connectSqlite3 "snaplets/hdbc/foo.db"
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    d <- nestSnaplet "hdbc" database $ hdbcInit conn
    addRoutes routes
    return $ App h d


