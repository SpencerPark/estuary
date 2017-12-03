{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.Database where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok

import Data.Map
import Data.Time.Clock
import Text.JSON
import Data.Text

import Estuary.Types.View
import Estuary.Types.Ensemble

openDatabase :: IO Connection
openDatabase = do
  c <- open "Estuary.db"
  createLogTable c
  createEnsembleTable c
  postLogToDatabase c "database opened"
  return c

createEnsembleTable :: Connection -> IO ()
createEnsembleTable c = execute_ c "CREATE TABLE IF NOT EXISTS ensembles (name TEXT, json TEXT, lastUpdate TEXT)"

createLogTable :: Connection -> IO ()
createLogTable c = execute_ c "CREATE TABLE IF NOT EXISTS log (time TEXT,msg TEXT)"

postLogToDatabase :: Connection -> String -> IO ()
postLogToDatabase c l = do
  now <- getCurrentTime
  execute c "INSERT INTO log (time,msg) VALUES (?,?)" (now,l)

writeEnsemble :: Connection -> String -> Ensemble -> IO ()
writeEnsemble c eName e = do
  now <- getCurrentTime
  execute c "UPDATE ensembles (json,lastUpdate) VALUES (?,?) WHERE name=?" (e,now,eName)

-- *** note: QUERY in above is almost certainly syntactically incorrect... ***

instance ToField Ensemble where
  toField = SQLText . pack . encode

instance FromField Ensemble where
  fromField = f . decode . g . fieldData
    where g (SQLText t) = unpack t
          g _ = ""
          f (Text.JSON.Ok x) = Database.SQLite.Simple.Ok.Ok x
          -- *** incomplete pattern matching here is a bad bad bad temporary idea...

readEnsembles :: Connection -> IO (Map String Ensemble)
readEnsembles c = do
  r <- query_ c "SELECT name,json FROM ensembles" -- [(n,j)]
  return $ fromList r

closeDatabase :: Connection -> IO ()
closeDatabase = close
