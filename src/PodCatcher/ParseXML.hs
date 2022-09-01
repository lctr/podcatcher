module PodCatcher.ParseXML where

import           PodCatcher.Types

import           Text.XML.HaXml
import           Text.XML.HaXml.Html.Generate (showattr)
import           Text.XML.HaXml.Parse
import           Text.XML.HaXml.Posn

import           Data.Char
import           Data.List

data PodItem = PodItem {
  item_title    :: String,
  enclosure_url :: String
} deriving (Eq, Show, Read)

data Feed = Feed {
  channel_title :: String,
  items         :: [PodItem]
} deriving (Eq, Show, Read)

-- | Produces an 'Episode' for a given podcast and a 'PodItem'.
itemToEpisode :: Podcast -> PodItem -> Episode
itemToEpisode podcast item = Episode {
  epId = 0,
  epCast = podcast,
  epURL = enclosure_url item,
  epDone = False
}

-- | Parse the data from a given string, with the given name to use in
-- error messages.
parse :: String -> String -> Feed
parse content name =
  Feed { channel_title = getTitle doc, items = getEnclosures doc }
  where
    parseResult = xmlParse name (stripUnicodeBOM content)
    doc = getContent parseResult
    getContent (Document _ _ e _) = CElem e noPos
    stripUnicodeBOM x = case x of
      ('\xfeff':x')             -> x'
      ('\xef':'\xbb':'\xbf':x') -> x'
      x'                        -> x'

-- | Pulls out the channel part of the document.
channel :: CFilter Posn
channel = tag "rss" /> tag "channel"

getTitle :: Content Posn -> String
getTitle doc = contentToStringDefault "Untitled Podcast"
  (channel /> tag "title" /> txt $ doc)

getEnclosures :: Content Posn -> [PodItem]
getEnclosures doc = concatMap procPodItem $ getPodItems doc
  where
    procPodItem item = concatMap (procEnclosure title) enclosure
      where title = contentToStringDefault "Untitled Episode"
              (keep /> tag "title" /> txt $ item)
            enclosure = (keep /> tag "enclosure") item

    getPodItems :: CFilter Posn
    getPodItems = channel /> tag "item"

    procEnclosure :: String -> Content Posn -> [PodItem]
    procEnclosure title enclosure =
        map mkPodItem (showattr "url" enclosure)
      where mkPodItem x = PodItem
              { item_title = title
              , enclosure_url = contentToString [x]
              }

-- | Converts a list of 'Content's to a printable 'String', with a
-- default if the passed in list is empty, signifying a lack of
-- a match.
contentToStringDefault :: String -> [Content Posn] -> String
contentToStringDefault msg [] = msg
contentToStringDefault _ x    = contentToString x

-- | Converts a list of 'Content' to a printable string, taking care
-- to unescape its text.
--
-- An implementation without unescaping would simply be:
--
-- @
-- contentToString = concatMap (show . content)
-- @
--
-- Because HaXml's unescaping only works on 'Element's, we must make
-- sure that whatever 'Content' we have is wrapped in an 'Element',
-- then use the 'txt' combinator to pull the insides back out.
contentToString :: [Content Posn] -> String
contentToString = concatMap procContent
  where
    procContent x =
      verbatim $ keep /> txt $ CElem (unesc (fakeElem x)) noPos
    fakeElem x = Elem (N "fake") [] [x]
    unesc = xmlUnEscape stdXmlEscaper
