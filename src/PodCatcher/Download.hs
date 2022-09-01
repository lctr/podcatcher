module PodCatcher.Download where

import           PodCatcher.Database
import           PodCatcher.ParseXML
import           PodCatcher.Types

import           Data.Maybe
import           Database.HDBC
import           Network.HTTP
import           Network.URI
import           System.IO

-- | Downloads a URL. Note that the HTTP is NOT read lazily, which may
-- lead to the consumption of a large amount of RAM when downloading
-- large files.
downloadURL :: String -> IO (Either String String)
downloadURL url = do
  resp <- simpleHTTP request
  case resp of
    Left x -> return . Left $ "Error connecting: " ++ show x
    Right r -> case rspCode r of
      (2, _, _) -> return . Right . rspBody $ r
      (3, _, _) -> -- HTTP redirect
        case findHeader HdrLocation r of
          Nothing  -> return $ Left (show r)
          Just url -> downloadURL url
      _ -> return . Left . show $ r
  where uri = fromJust $ parseURI url
        request = Request
          { rqURI = uri
          , rqMethod = GET
          , rqHeaders = []
          , rqBody = ""
          }

-- | Updates the given podcast in the database.
updatePodcastFromFeed :: IConnection conn => conn -> Podcast -> IO ()
updatePodcastFromFeed dbh pc =
  downloadURL (castURL pc) >>= either putStrLn updateDB
  where
    updateDB doc = mapM_ (addEpisode dbh) episodes >> commit dbh
      where feed = parse doc (castURL pc)
            episodes = map (itemToEpisode pc) (items feed)

-- | Downloads an episode, returning a 'String' representing the
-- filename into which it was placed if successful, or 'Nothing'
-- otherwise.
getEpisode :: IConnection conn => conn -> Episode -> IO (Maybe String)
getEpisode dbh ep = undefined
  where
    filename = "pod."
      ++ (show . castId . epCast) ep
      ++ "." ++ (show . epId) ep ++ ".mp3"
    save doc = let name = filename
      in do file <- openBinaryFile name WriteMode
            hPutStr file doc
            hClose file
            updateEpisode dbh (ep { epDone = True })
            commit dbh
            return (Just name)
