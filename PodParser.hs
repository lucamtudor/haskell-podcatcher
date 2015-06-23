module PodParser where

import           Text.XML.HaXml
import           Text.XML.HaXml.Posn

import           PodTypes

data PodItem = PodItem {
      itemTitle    :: String
    , enclosureURL :: String
    } deriving (Eq, Show, Read)

data Feed = Feed {
      channelTitle :: String
    , items        :: [PodItem]
    } deriving (Eq, Show, Read)

{- | Given a podcast and an PodItem, produce an Episode -}
item2episode :: Podcast -> PodItem -> Episode
item2episode p item = Episode { epId = 0,
                                epCast = p,
                                epURL = enclosureURL item,
                                epDone = False }

{- | Parse the data from a given string, with the given name to use
in error messages. -}
parse :: String -> String -> Feed
parse content name = Feed { channelTitle = getTitle doc,
                            items = getEnclosures doc}
  where
    doc = getContent parseResults
    parseResults = xmlParse name $ stripUnicodeBOM content

    getContent :: Document Posn -> Content Posn
    getContent (Document _ _ e _) = CElem e noPos

    stripUnicodeBOM :: String -> String
    stripUnicodeBOM ('\xfeff':a) = a
    stripUnicodeBOM a = a

{- | Pull out the channel part of the document.-}
channel :: CFilter Posn
channel = tag "rss" /> tag "channel"

getTitle :: Content Posn -> String
getTitle doc = content2StrWithDef c "Untitled Podcast"
  where
    c = channel /> tag "title" /> txt $ doc

getEnclosures :: Content Posn -> [PodItem]
getEnclosures doc = concatMap procPodItem $ getPodItems doc
 where
    procPodItem :: Content Posn -> [PodItem]
    procPodItem item = concatMap (procEnclosure title) enclosure
      where
        title = content2StrWithDef (keep /> tag "title" /> txt $ item)
            "Untitled Episode"
        enclosure = keep /> tag "enclosure" $ item

    getPodItems :: CFilter Posn
    getPodItems = channel /> tag "item"

    procEnclosure :: String -> Content Posn -> [PodItem]
    procEnclosure title enclosure = map makePodItem (showattr "url" enclosure)
      where
        makePodItem :: Content Posn -> PodItem
        makePodItem a = PodItem { itemTitle = title,
                                  enclosureURL = content2Str [a]}

{- | Convert [Content] to a printable String, with a default if the
passed-in [Content] is [], signifying a lack of a match. -}
content2StrWithDef :: [Content Posn] -> String -> String
content2StrWithDef [] defaultMsg = defaultMsg
content2StrWithDef a _           = content2Str a

{- | Convert [Content] to a printable string, taking care to unescape it.

An implementation without unescaping would simply be:

> contentToString = concatMap (show . content)

Because HaXml's unescaping only works on Elements, we must make sure that
whatever Content we have is wrapped in an Element, then use txt to
pull the insides back out. -}
content2Str :: [Content Posn] -> String
content2Str = concatMap processContent
  where
    processContent a = verbatim $ keep /> txt $ CElem (unesc (fakeElem a)) noPos

    fakeElem :: Content Posn -> Element Posn
    fakeElem a = Elem (N "fake") [] [a]

    unesc :: Element Posn -> Element Posn
    unesc = xmlUnEscape stdXmlEscaper
