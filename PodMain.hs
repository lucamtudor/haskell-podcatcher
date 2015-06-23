module Main where

import           Database.HDBC
import           Network.Socket     (withSocketsDo)
import           System.Environment

import           PodDB
import           PodDownload
import           PodTypes

main :: IO ()
main = withSocketsDo $ handleSqlError $ do
    args <- getArgs
    dbconn <- connect "pod.db"
    case args of
        ["help"] -> help
        ["add", url] -> add dbconn url
        ["update"] -> update dbconn
        ["download"] -> download dbconn
        ["fetch"] -> do { update dbconn; download dbconn }
        _ -> syntaxError
    disconnect dbconn

add :: IConnection conn => conn -> String -> IO ()
add dbconn url = do
    _ <- addPodcast dbconn podcast
    commit dbconn
  where
    podcast = Podcast { castId = 0, castURL = url }

update :: IConnection conn => conn -> IO ()
update dbconn = do
    podcasts <- getPodcasts dbconn
    mapM_ procPodcast podcasts
  where
    procPodcast p = do
        putStrLn $ "Updating from " ++ castURL p
        updatePodcastFromFeed dbconn p

download :: IConnection conn => conn -> IO ()
download dbconn = do
    podcasts <- getPodcasts dbconn
    mapM_ procPodcast podcasts
  where
    procPodcast p = do
        putStrLn $ "Considering " ++ castURL p
        episodes <- getPodcastEpisodes dbconn p
        let episodesToDownload = filter (not . epDone) episodes
        mapM_ procEpisode episodesToDownload
    procEpisode ep = do
        putStrLn $ "Downloading " ++ epURL ep
        getEpisode dbconn ep

syntaxError :: IO ()
syntaxError = do
    putStrLn "Don't be an idiot."
    help

help :: IO ()
help = putStrLn
  "Usage: podcatch command [args]\n\
  \\n\
  \podcatch add url      Adds a new podcast with the given URL\n\
  \podcatch download     Downloads all pending episodes\n\
  \podcatch fetch        Updates, then downloads\n\
  \podcatch update       Downloads podcast feeds, looks for new episodes\n"
