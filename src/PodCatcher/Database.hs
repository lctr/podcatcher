module PodCatcher.Database where

import           Database.HDBC
import           Database.HDBC.Sqlite3

import           PodCatcher.Types

import           Control.Monad         (unless, void, when)
import           Data.Functor          ((<&>))
import           Data.List             (sort)

-- | Initializes the database and returns a database connection
connect :: FilePath -> IO Connection
connect fp = do
  dbh <- connectSqlite3 fp
  prepDB dbh
  return dbh

-- | Prepares the database for the PodCatcher's data
--
-- This consists of creating two tables and asking the database engine
-- to verify some piecess of data consistency, namely:
--
-- * 'cast_id' and 'ep_id' are both unique primary keys which must never
--   be duplicated
-- * 'cast_url' is also unique
-- * There must only be one instance of each given URL or episode ID
--   in the episodes table for a given podcast ('epcast')
prepDB :: IConnection conn => conn -> IO ()
prepDB dbh = do
    tables <- getTables dbh
    unless ("podcasts" `elem` tables) $
      void (run dbh createPodcastsTableQuery [])
    unless ("episodes" `elem` tables) $
      void (run dbh createEpisodesTableQuery [])
    commit dbh
  where
    createPodcastsTableQuery =
      "CREATE TABLE podcasts (\
       \cast_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
       \cast_url TEXT NOT NULL UNIQUE\
      \)"
    createEpisodesTableQuery =
      "CREATE TABLE episodes (\
       \ep_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
       \ep_cast_id INTEGER NOT NULL,\
       \ep_url TEXT NOT NULL,\
       \ep_done INTEGER NOT NULL,\
       \UNIQUE(ep_cast_id, ep_url),\
       \UNIQUE(ep_cast_id, ep_id)\
      \)"

-- | Adds a new podcast to the database. Ignores the 'cast_id' on the
-- incoming podcast and returns a new object with the 'cast_id'
-- populated.
--
-- Returns an error if there is an attempt to add a podcast that
-- already exists.
addPodcast :: IConnection conn => conn -> Podcast -> IO Podcast
addPodcast dbh podcast = handleSql handleError $ do
  -- Insert the 'cast_url' into the table. the database will
  -- automattically assign it a cast ID.
  run dbh insertCastURLQuery [sqlPodcastURL]
  -- Find out the 'cast_id' for the newly added URL
  r <- quickQuery' dbh selectCastIdQuery [sqlPodcastURL]
  case r of
    [[x]] -> return (podcast { castId = fromSql x})
    y     -> reportUnexpected y
  where
    sqlPodcastURL = toSql (castURL podcast)
    insertCastURLQuery =
      "INSERT INTO podcasts (cast_url) VALUES (?)"
    selectCastIdQuery =
      "SELECT cast_id FROM podcasts WHERE cast_URL = ?"
    reportUnexpected res =
      fail $ "addPodcast: unexpected result: " ++ show res
    handleError e = do fail $ "Error adding podcast; \
      \does the following URL already exist?\n" ++ show e

-- | Adds a new podcast episode to the database.
--
-- Since this is automated and not triggered by user request, it will
-- ignore requests to add duplicate episodes. This way, when feeds are
-- processed, each URL encountered can be fed to this function without
-- having to first look it up in the database.
--
-- Additionally, the new ID for a podcast is not generally relevant
-- here, so it won't be fetched from the database.
addEpisode :: IConnection conn => conn -> Episode -> IO ()
addEpisode dbh ep = void (run dbh sql values)
  where
    sql =
      "INSERT OR IGNORE INTO episodes (ep_cast_id, ep_url, ep_done) \
        \VALUES (?, ?, ?)"
    values =
      [ toSql (castId . epCast $ ep)
      , toSql (epURL ep)
      , toSql (epDone ep)
      ]

-- | Modifies an existing podcast. Looks up the given podcast by ID
-- and updates the database record to match the passed Podcast.
updatePodcast :: IConnection conn => conn -> Podcast -> IO ()
updatePodcast dbh podcast = void (run dbh sql values)
  where
    sql = "UPDATE podcasts SET cast_url = ? WHERE cast_id = ?"
    values = [toSql (castURL podcast), toSql (castId podcast)]

-- | Modifies an existing podcast episode. Looks up a podcast episode
-- by ID and modifies the database record to match the given episode.
updateEpisode :: IConnection conn => conn -> Episode -> IO ()
updateEpisode dbh episode = void (run dbh sql values)
  where
    sql = "UPDATE episodes SET \
          \ep_cast_id = ?, ep_url = ?, ep_done = ? \
          \WHERE ep_id = ?"
    values =
      [ toSql (castId . epCast $ episode)
      , toSql (epURL episode)
      , toSql (epDone episode)
      , toSql (epId episode)
      ]

-- | Removes a podcast. First removes any episodes that may exist for
-- the given podcast.
removePodcast :: IConnection conn => conn -> Podcast -> IO ()
removePodcast dbh podcast = mapM_ (runDelete podcast) tableFields
  where
    runDelete p = flip (run dbh) [toSql . castId $ p] . sql
    sql (tbl, field) =
      "DELETE FROM " ++ tbl ++ " WHERE " ++ field ++ " = ?"
    tableFields =
      [ ("episodes", "ep_cast_id")
      , ("podcasts", "cast_id")
      ]

-- | Retrieves a list of all podcasts.
getPodcasts :: IConnection conn => conn -> IO [Podcast]
getPodcasts dbh =
  let sql = "SELECT cast_id, cast_url \
            \FROM podcasts \
            \ORDER BY cast_id"
  in quickQuery' dbh sql [] <&> map rowToPodcast

-- | Converts the result of a @SELECT cast_id, cast_url FROM podcasts
-- ...@ query into a 'Podcast'.
rowToPodcast :: [SqlValue] -> Podcast
rowToPodcast [svId, svURL] = Podcast {
  castId = fromSql svId,
  castURL = fromSql svURL }
rowToPodcast row = error $
  "Can't convert row " ++ show row ++ " into a Podcast"

-- | Retrieves a list of all the episodes for a particular podcast.
getPodcastEpisodes :: IConnection c => c -> Podcast -> IO [Episode]
getPodcastEpisodes dbh podcast =
  quickQuery' dbh sql [cast_id] <&> map rowToEpisode
  where
    sql = "SELECT ep_id, ep_url, ep_done \
          \FROM episodes \
          \WHERE ep_cast_id = ?"
    cast_id = toSql . castId $ podcast
    rowToEpisode [svId, svURL, svDone]
      = Episode {
        epId = fromSql svId,
        epURL = fromSql svURL,
        epDone = fromSql svDone,
        epCast = podcast }
    rowToEpisode row
      = error $
        "Can't convert row " ++ show row ++ " into an Episode"
