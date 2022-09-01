module Main where

import           PodCatcher

import           Database.HDBC
import           Network.Socket     (withSocketsDo)
import           System.Environment

main :: IO ()
main = withSocketsDo $ handleSqlError $ do
  args <- getArgs
  dbh <- connect "pod.db"
  case args of
    ["add", url] -> add dbh url
    ["update"]   -> update dbh
    ["download"] -> download dbh
    ["fetch"]    -> update dbh >> download dbh
    ["help"]     -> putStrLn usage
    [cmd]        -> unknownCommand cmd
    _            -> putStrLn usage
  disconnect dbh

add :: IConnection conn => conn -> [Char] -> IO ()
add dbh url =
  let podcast = Podcast {castId = 0, castURL = url}
  in addPodcast dbh podcast >> commit dbh

update :: IConnection conn => conn -> IO ()
update dbh =
  let procPodcast pc = putStrLn ("Updating from " ++ castURL pc) >> updatePodcastFromFeed dbh pc in getPodcasts dbh >>= mapM_ procPodcast

download :: IConnection conn => conn -> IO ()
download dbh = getPodcasts dbh >>= mapM_ procPodcast
  where
    procPodcast pc = do
      putStrLn ("Considering " ++ castURL pc)
      episodeList <- getPodcastEpisodes dbh pc
      let dleps = filter (not . epDone) episodeList
      mapM_ procEpisode dleps
    procEpisode ep = putStrLn ("Downloading " ++ epURL ep)
      >> getEpisode dbh ep


unknownCommand :: String -> IO ()
unknownCommand cmd = do
  putStrLn $ "Unknown command `" ++ cmd ++ "`\n\n"
  putStrLn usage

usage :: String
usage = "Usage: PodCatcher command [args]\n\
  \\n\
  \PodCatcher add url      Adds a new podcast with the given URL\n\
  \PodCatcher download     Downloads all pending episodes\n\
  \PodCatcher fetch        Updates then downloads\n\
  \PodCatcher update       Downloads podcast feeds, looks for new episodes\n"
