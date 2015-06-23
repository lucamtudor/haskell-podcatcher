module PodTypes where

data Podcast = Podcast {
      castId  :: Integer
    , castURL :: String
    } deriving (Eq, Show, Read)

data Episode = Episode {
      epId   :: Integer
    , epCast :: Podcast
    , epURL  :: String
    , epDone :: Bool
    } deriving (Eq, Show, Read)
