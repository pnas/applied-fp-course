{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Level07.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO, MonadIO (..))
import           Control.Monad.Reader               (MonadReader (..) , asks)

import           Data.Bifunctor                     (first, bimap)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level07.AppM                      (AppM(..), App, Env (envDB), liftEither)

import           Level07.Types                     (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError),
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

getDBConn :: MonadReader Env App
  => App Connection
getDBConn =
  asks (dbConn . envDB)
  -- reader (dbConn . envDB)
  -- error "getDBConn not implemented"

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> App b
runDB f a = AppM $ \ z -> do
  cc <- runAppM getDBConn z
  case cc of 
    Left e -> return $ Left e 
    Right p -> do 
      r <- liftIO $ first DBError <$> ((Sql.runDBAction . a) p)
      return $ either Left f r      

-- without explicit reader  
-- runDB f a = AppM $ \ z -> do
--   r <- liftIO $ first DBError <$> Sql.runDBAction (a (dbConn (envDB z)))
--   return $ either Left f r
  
  -- ( \z -> f =<<
  --   (first DBError <$> Sql.runDBAction (a (dbConn (envDB z))) ) )

  -- error "runDB not re-implemented"

getComments
  :: Topic
  -> App [Comment]
getComments topic = do
  -- Write the query with an icky string and remember your placeholders!
  let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  -- To be doubly and triply sure we've no garbage in our response, we take care
  -- to convert our DB storage type into something we're going to share with the
  -- outside world. Checking again for things like empty Topic or CommentText values.
  runDB (traverse fromDBComment) ( \ db -> Sql.query db q (Sql.Only . getTopic $ topic))

  -- error "Copy your completed 'getComments' and refactor to match the new type signature"

addCommentToTopic
  :: Topic
  -> CommentText
  -> App ()
addCommentToTopic t c = do
  -- Record the time this comment was created.
  nowish <- liftIO getCurrentTime
  -- Note the triple, matching the number of values we're trying to insert, plus
  -- one for the table name.
  let q =
        -- Remember that the '?' are order dependent so if you get your input
        -- parameters in the wrong order, the types won't save you here. More on that
        -- sort of goodness later.
        "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  -- We use the execute function this time as we don't care about anything
  -- that is returned. The execute function will still return the number of rows
  -- affected by the query, which in our case should always be 1.
  runDB Right (\db -> Sql.execute db q (getTopic t, getCommentText c, nowish) )
  -- An alternative is to write a returning query to get the Id of the DBComment
  -- we've created. We're being lazy (hah!) for now, so assume awesome and move on.

  -- error "Copy your completed 'appCommentToTopic' and refactor to match the new type signature"

getTopics
  :: App [Topic]
getTopics =
  let q = "SELECT DISTINCT topic FROM comments"
  in
    runDB (traverse ( mkTopic . Sql.fromOnly )) (\db -> Sql.query_ db q )

  -- error "Copy your completed 'getTopics' and refactor to match the new type signature"

deleteTopic
  :: Topic
  -> App ()
deleteTopic  t =
  let q = "DELETE FROM comments WHERE topic = ?"
  in
    runDB Right (\db -> Sql.execute db q (Sql.Only . getTopic $ t) )

  -- error "Copy your completed 'deleteTopic' and refactor to match the new type signature"

-- Go on to 'src/Level07/Core.hs' next.
