{-# LANGUAGE OverloadedStrings #-}
module Level05.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first)
import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.Types                      (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           Level05.AppM                       (AppM(..))

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB
  :: (a -> Either Error b)
  -> IO a
  -> AppM b
runDB f ioa = AppM $ do a <- ioa
                        return . f $ a

getComments
  :: FirstAppDB
  -> Topic
  -> AppM [Comment]
getComments appDb topic =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  in
    AppM $ do dbComments <- Sql.query (dbConn appDb) sql (Sql.Only $ getTopic topic)
              return . sequence $ fromDBComment <$> dbComments

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> AppM ()
addCommentToTopic appDb topic comment =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  in
    liftIO $ do currentTime <- getCurrentTime
                Sql.execute (dbConn appDb) sql (getTopic topic, getCommentText comment, currentTime)
                return ()

getTopics
  :: FirstAppDB
  -> AppM [Topic]
getTopics appDb =
  let
    sql = "SELECT DISTINCT topic FROM comments"
  in
    AppM $ do topics <- Sql.query_ (dbConn appDb) sql
              return $ traverse (mkTopic . Sql.fromOnly) topics

deleteTopic
  :: FirstAppDB
  -> Topic
  -> AppM ()
deleteTopic appDb topic =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
  in
    liftIO $ Sql.execute (dbConn appDb) sql (Sql.Only $ getTopic topic) >> (return ())

