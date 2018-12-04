{-# LANGUAGE OverloadedStrings #-}
module Level07.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Data.Bifunctor                     (first, bimap)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level07.AppM                      (AppM(AppM), Env (envDB, envConfig), liftEither)

import           Level07.Types                     (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError), Conf(..),
                                                     FirstAppDB (FirstAppDB, dbConn),
                                                     Topic, fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: AppM Connection
getDBConn = AppM $ \env -> do db <- initDB (dbFilePath . envConfig $ env)
                              return $ bimap DBError dbConn db

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> AppM b
runDB f g = getDBConn >>= liftIO . g >>= liftEither . f

getComments
  :: Topic
  -> AppM [Comment]
getComments topic = runDB f g
  where f = traverse fromDBComment
        g conn = let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
                 in Sql.query conn q (Sql.Only . getTopic $ topic)

addCommentToTopic
  :: Topic
  -> CommentText
  -> AppM ()
addCommentToTopic t c = runDB f g
  where f = Right
        g conn = do nowish <- getCurrentTime
                    let q = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
                    Sql.execute conn q (getTopic t, getCommentText c, nowish)

getTopics
  :: AppM [Topic]
getTopics = runDB f g
  where f = traverse(mkTopic . Sql.fromOnly)
        g = let q = "SELECT DISTINCT topic FROM comments" in flip Sql.query_ q

deleteTopic
  :: Topic
  -> AppM ()
deleteTopic t = runDB f g
  where f = Right
        g conn = let q = "DELETE FROM comments WHERE topic = ?" in Sql.execute conn q (Sql.Only . getTopic $ t)

-- Go on to 'src/Level07/Core.hs' next.
