{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
--   handler monad.
--
module Application where

------------------------------------------------------------------------------
import           Control.Monad.State
import           Data.Lens.Template
import           Database.HDBC.Sqlite3
------------------------------------------------------------------------------
import           Snap.Snaplet
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Heist

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _database :: Snaplet (HdbcSnaplet Connection IO)
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasHdbc (Handler App App) Connection IO where
  getHdbcState = with database get


------------------------------------------------------------------------------
type AppHandler = Handler App App


