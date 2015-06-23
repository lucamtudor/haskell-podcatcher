module PodDownload where


import           Data.Maybe
import           Database.HDBC
import           Network.HTTP
import           Network.URI
import           System.IO

import           PodDB
import           PodParser
import           PodTypes


downloadURL :: String -> IO (Either String String)
downloadURL url = do
    resp <- simpleHTTP request
    case resp of
        Left x -> return $ Left ("Error connecting: " ++ show x)
        Right r -> case rspCode r of
                       (2,_,_) -> return $ Right $ rspBody r
                       (3,_,_) -> redirect r
                       _       -> return $ Left $ show r
  where
    request = Request { rqURI = uri,
                        rqMethod = GET,
                        rqHeaders = [],
                        rqBody = ""}
    uri = fromJust $ parseURI url
    redirect r = case findHeader HdrLocation r of
                     Nothing          -> return $ Left $ show r
                     Just redirectUrl -> downloadURL redirectUrl

{- | Update the podcast in the database. -}
updatePodcastFromFeed :: IConnection conn => conn -> Podcast -> IO ()
updatePodcastFromFeed dbconn p = do
    resp <- downloadURL $ castURL p
    case resp of
        Left x -> putStrLn x
        Right doc -> updateDB doc
  where
    updateDB doc = do
        addEpisode dbconn `mapM_` episodes
        commit dbconn
      where
        episodes = map (item2episode p) (items feed)
        feed = parse doc $ castURL p

getEpisode :: IConnection conn => conn -> Episode -> IO (Maybe String)
getEpisode dbconn ep = do
    resp <- downloadURL $ epURL ep
    case resp of
        Left x    -> do { putStrLn x; return Nothing }
        Right doc -> do
            file <- openBinaryFile filename WriteMode
            hPutStr file doc
            hClose file
            updateEpisode dbconn $ ep { epDone = True}
            commit dbconn
            return $ Just filename
  where
    filename = "pod." ++ (show . castId .epCast $ ep) ++ "." ++ show (epId ep)
        ++ ".mp3"
