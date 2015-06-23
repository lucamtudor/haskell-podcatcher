module PodDB where

import           Control.Monad         (unless, void)
import           Database.HDBC
import           Database.HDBC.Sqlite3

import           PodTypes

-- | Initialize DB and return database Connection
connect :: FilePath -> IO Connection
connect dbfp = do
    dbconn <- connectSqlite3 dbfp
    prepDB dbconn
    return dbconn

{- | Prepare the database for our data.

We create two tables and ask the database engine to verify some pieces
of data consistency for us:

* castid and epid both are unique primary keys and must never be duplicated
* castURL also is unique
* In the episodes table, for a given podcast (epcast), there must be only
  one instance of each given URL or episode ID
-}
prepDB :: IConnection conn => conn -> IO ()
prepDB dbconn = do
    tables <- getTables dbconn
    unless (podcastsTableName `elem` tables) $ do
        _ <- run dbconn podcastsTableCreate []
        return ()
    unless (episodesTableName `elem` tables) $ do
        _ <- run dbconn episodesTableCreate []
        return ()
    commit dbconn

{- | Adds a new podcast to the database.  Ignores the castid on the
incoming podcast, and returns a new object with the castid populated.

An attempt to add a podcast that already exists is an error. -}
addPodcast :: IConnection conn => conn -> Podcast -> IO Podcast
addPodcast dbconn p = handleSql errHandler $ do
    _ <- run dbconn podcastsTableInsert [toSql $ castURL p]
    r <- quickQuery' dbconn podcastsTableSelectIdByURL [toSql $ castURL p]
    case r of
        [[pId]] -> return $ p {castId = fromSql pId}
        err -> fail $ "addPodcast: unexpected result: " ++ show err
  where
    errHandler e = fail $ "Error adding podcast; does this URL already exist?\n"
        ++ show e

{- | Modifies an existing podcast.  Looks up the given podcast by
ID and modifies the database record to match the passed Podcast. -}
updatePodcast :: IConnection conn => conn -> Podcast -> IO ()
updatePodcast dbconn p =
    void $ run dbconn podcastsTableUdate [podcastURL, podcastId]
  where
    podcastURL = toSql $ castURL p
    podcastId = toSql $ castId p

{- | Remove a podcast.  First removes any episodes that may exist
for this podcast. -}
removePodcast :: IConnection conn => conn -> Podcast -> IO ()
removePodcast dbconn p = do
    _ <- run dbconn podcastsTableDelete [podcastId]
    removeEpisodesForPodcast dbconn podcastId
  where
    podcastId = toSql $ castId p

{- | Gets a list of all podcasts. -}
getPodcasts :: IConnection conn => conn -> IO [Podcast]
getPodcasts dbconn = do
    res <- quickQuery' dbconn podcastsTableSelectAll []
    return $ map convPodcastRow res

{- | Get a particular podcast.  Nothing if the ID doesn't match, or
Just Podcast if it does. -}
getPodcast :: IConnection conn => conn -> Integer -> IO (Maybe Podcast)
getPodcast dbconn podcastId = do
    res <- quickQuery' dbconn podcastsTableSelectById [toSql podcastId]
    case res of
        [row] -> return $ Just (convPodcastRow row)
        []    -> return Nothing
        x     -> fail $ "You messed up: more than one podcast with ID "
                    ++ show podcastId ++ ".\n" ++ show x

convPodcastRow :: [SqlValue] -> Podcast
convPodcastRow [savedId, savedURL] =
    Podcast {castId = fromSql savedId, castURL = fromSql savedURL}
convPodcastRow x = error $ "Can't convert podcast row " ++ show x

{- | Adds a new episode to the database.

Since this is done by automation, instead of by user request, we will
simply ignore requests to add duplicate episodes.  This way, when we are
processing a feed, each URL encountered can be fed to this function,
without having to first look it up in the DB.

Also, we generally won't care about the new ID here, so don't bother
fetching it. -}
addEpisode :: IConnection conn => conn -> Episode -> IO ()
addEpisode dbconn ep =
    void $ run dbconn episodesTableInsert values
  where
    values = [epCastIdToSql ep, epURLToSql ep, epDoneToSql ep]

{- | Modifies an existing episode.  Looks it up by ID and modifies the
database record to match the given episode. -}
updateEpisode :: IConnection conn => conn -> Episode -> IO ()
updateEpisode dbconn ep =
    void $ run dbconn episodesTableUpdate values
  where
    values = [epCastIdToSql ep, epURLToSql ep, epDoneToSql ep, episodeId]
    episodeId = toSql $ epId ep

{- | Remove any episodes for a particular podcast -}
removeEpisodesForPodcast :: IConnection conn => conn -> SqlValue -> IO ()
removeEpisodesForPodcast dbconn podcastId =
    void $ run dbconn episodesTableDeleteByPodcastId [podcastId]

{- | Get all episodes for a particular podcast. -}
getPodcastEpisodes :: IConnection conn => conn -> Podcast -> IO [Episode]
getPodcastEpisodes dbconn p = do
    rows <- quickQuery' dbconn episodesTableSelectByPodcastId [toSql $ castId p]
    return $ map convEpisodeRow rows
  where
    convEpisodeRow [savedId, savedURL, savedDone] =
        Episode {epId = fromSql savedId, epURL = fromSql savedURL,
            epDone = fromSql savedDone, epCast = p}

epCastIdToSql :: Episode -> SqlValue
epCastIdToSql ep = toSql (castId . epCast $ ep)

epURLToSql :: Episode -> SqlValue
epURLToSql ep = toSql $ epURL ep

epDoneToSql :: Episode -> SqlValue
epDoneToSql ep = toSql $ epDone ep

podcastsTableName :: String
podcastsTableName = "PODCASTS"

podcastsTableCreate :: String
podcastsTableCreate =
    "CREATE TABLE " ++ podcastsTableName ++ " (\
     \castId INTEGER PRIMARY KEY AUTOINCREMENT,\
     \castURL TEXT NOT NULL UNIQUE)"

podcastsTableInsert :: String
podcastsTableInsert =
    "INSERT INTO " ++ podcastsTableName ++ " (castURL) VALUES (?)"

podcastsTableUdate :: String
podcastsTableUdate =
    "UPDATE " ++ podcastsTableName ++ " SET castURL = ? WHERE castId = ?"

podcastsTableDelete :: String
podcastsTableDelete = "DELETE FROM " ++ podcastsTableName ++ " WHERE castId = ?"

podcastsTableSelectAll :: String
podcastsTableSelectAll =
    "SELECT castId, castURL FROM " ++ podcastsTableName ++ " ORDER BY castId"

podcastsTableSelectById :: String
podcastsTableSelectById =
    "SELECT castId, castURL FROM " ++ podcastsTableName ++ " WHERE castId = ?"

podcastsTableSelectIdByURL :: String
podcastsTableSelectIdByURL =
    "SELECT castId FROM " ++ podcastsTableName ++ " WHERE castURL = ?"

episodesTableName :: String
episodesTableName = "EPISODES"

episodesTableCreate :: String
episodesTableCreate =
    "CREATE TABLE " ++ episodesTableName ++ " (\
        \epId INTEGER PRIMARY KEY AUTOINCREMENT,\
        \epCastId INTEGER NOT NULL,\
        \epURL TEXT NOT NULL,\
        \epDone INTEGER NOT NULL,\
        \UNIQUE(epcastid, epURL),\
        \UNIQUE(epcastid, epId))"

episodesTableInsert :: String
episodesTableInsert =
    "INSERT OR IGNORE INTO " ++ episodesTableName ++
    " (epCastId, epURL, epDone) VALUES (?, ?, ?)"

episodesTableUpdate :: String
episodesTableUpdate =
    "UPDATE " ++ episodesTableName ++ " SET epCastId = ?, epURL = ?, epDone = ?\
    \ WHERE epId = ?"

episodesTableDeleteByPodcastId :: String
episodesTableDeleteByPodcastId =
    "DELETE FROM " ++ episodesTableName ++ " WHERE epCastId = ?"

episodesTableSelectByPodcastId :: String
episodesTableSelectByPodcastId =
    "SELECT epId, epURL, epDone FROM " ++ episodesTableName ++
    " WHERE epCastId =?"
