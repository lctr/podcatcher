module PodCatcher.Types where

data Podcast = Podcast {
  castId  :: Integer, -- ^ Unique numeric ID for a podcast
  castURL :: String -- ^ The feed URL for a podcast
} deriving (Eq, Show, Read)

data Episode = Episode{
  epId   :: Integer, -- ^ Numeric ID for the episode
  epCast :: Podcast, -- ^ The ID of the podcast it came from
  epURL  :: String, -- ^ The download URL for the episode
  epDone :: Bool -- ^ Whether the episode has been completed
} deriving (Eq, Show, Read)
