module Horrsubs.Lib
  ( fetchMkvPaths
  , makeVideoProperty
  , extractSubtitles
  , exampleTargetFolder
  ) where

import           Control.Applicative
import qualified Control.Exception as EX
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Maybe
import qualified Data.Text as T
import           System.Directory
import           System.FilePath
import           System.Process

data VideoProperty = VideoProperty
  { videoPropertyPath :: FilePath
  , videoPropertyInfos :: [VideoInfo]
  } deriving (Show)

data VideoInfo
  = VideoInfoMetaData VideoMetaData
  | VideoInfoTrack VideoTrack
  | VideoInfoAttachment VideoAttachment
  | VideoInfoChapter VideoChapter
  | VideoInfoTrackTag VideoTrackTag
  deriving (Show)

data VideoMetaData = VideoMetaData
  { videoMetaDataFileName :: T.Text
  , videoMetaDataContainer :: T.Text
  } deriving (Show)

data VideoTrack = VideoTrack
  { videoTrackId :: VideoTrackId
  , videoTrackKind :: TrackKind
  , videoTrackFormat :: T.Text
  } deriving (Show)

newtype VideoTrackId = VideoTrackId
  { unVideoTrackId :: Int
  } deriving (Show)

data VideoAttachment = VideoAttachment
  { videoAttachmentId :: Int
  , videoAttachmentMimeType :: T.Text
  , videoAttachmentSize :: Int
  , videoAttachmentDescription :: T.Text
  , videoAttachmentName :: T.Text
  } deriving (Show)

data VideoChapter = VideoChapter
  { videoChapterEntries :: Int
  } deriving (Show)

data VideoTrackTag = VideoTrackTag
  { videoTrackTagId :: Int
  , videoTrackTagEntries :: Int
  } deriving (Show)

data TrackKind = Video | Audio | Subtitles deriving (Show)

fetchMkvPaths :: FilePath -> IO [FilePath]
fetchMkvPaths path = fmap appendRootPath
  . filterMkv
  . concat
  . toMaybe <$> fetchedPaths
  where
    fetchedPaths :: IO (Either EX.SomeException [FilePath])
    fetchedPaths = putStrLn path >> EX.try (getDirectoryContents path)

    filterMkv :: [FilePath] -> [FilePath]
    filterMkv = filter $ isExtensionOf ".mkv"

    appendRootPath :: FilePath -> FilePath
    appendRootPath a = path <> "/" <> a

makeVideoProperty :: FilePath -> IO (Maybe VideoProperty)
makeVideoProperty path = do
  result <- toMaybe <$> infoString
  return $ fmap (VideoProperty path) . parse' . T.pack =<< result
  where
    infoString :: IO (Either EX.SomeException String)
    infoString = EX.try $ readProcess programPath ["-i", path] ""

    parse' = toMaybe . parseOnly (many1 $ videoInfoParser <* endOfLine)
    programPath = "/home/faris/.nix-profile/bin/mkvmerge"

extractSubtitles :: VideoProperty -> IO ()
extractSubtitles videoProp = mapM_ extract
  $ zipWith (,) [0..]
  $ catMaybes
  $ fmap asSubtitlesVideoTrack
  $ videoPropertyInfos videoProp
  where
    extract :: (Integer, VideoTrack) -> IO ()
    extract (idx, videoTrack) =
      let filePath = videoPropertyPath videoProp
          programPath = "/home/faris/.nix-profile/bin/mkvextract"
          strTrackId = show $ unVideoTrackId $ videoTrackId videoTrack
          srtPath = if idx == 0
            then replaceExtension filePath "srt"
            else replaceExtension filePath $ show idx <> ".srt"
       in do
         isExist <- doesFileExist srtPath
         if isExist
           then return ()
           else void $ (EX.try $ callProcess programPath
                         [ "tracks"
                         , filePath
                         , strTrackId <> ":" <> srtPath
                         ] :: IO (Either EX.SomeException ()))

    asSubtitlesVideoTrack :: VideoInfo -> Maybe VideoTrack
    asSubtitlesVideoTrack (VideoInfoTrack videoTrack) =
      case videoTrackKind videoTrack of
        Subtitles -> Just videoTrack
        _ -> Nothing
    asSubtitlesVideoTrack _ = Nothing

videoInfoParser :: Parser VideoInfo
videoInfoParser = choice
  [ VideoInfoMetaData <$> videoMetaDataParser
  , VideoInfoTrack <$> videoTrackParser
  , VideoInfoAttachment <$> videoAttachmentParser
  , VideoInfoChapter <$> videoChapterParser
  , VideoInfoTrackTag <$> videoTrackTagParser
  ]

videoMetaDataParser :: Parser VideoMetaData
videoMetaDataParser = do
  string "File" >> skipSpace >> skip isApostrophe
  fileName <- takeTill isApostrophe
  skip isApostrophe
    >> skip isColon
    >> skipSpace
    >> string "container"
    >> skip isColon
    >> skipSpace
  container <- takeTill isEndOfLine
  return $ VideoMetaData fileName container

videoTrackParser :: Parser VideoTrack
videoTrackParser = do
  string "Track" >> skipSpace >> string "ID" >> skipSpace
  id' <- VideoTrackId . digitToInt <$> digit
  skip isColon >> skipSpace
  kind <- trackKindParser
  skipSpace >> skip isOpenBracket
  format <- takeTill isCloseBracket
  skip isCloseBracket
  return $ VideoTrack id' kind format

videoAttachmentParser :: Parser VideoAttachment
videoAttachmentParser = do
  string "Attachment" >> skipSpace >> string "ID" >> skipSpace
  id' <- digitToInt <$> digit
  skip isColon
    >> skipSpace
    >> string "type"
    >> skipSpace
    >> skip isApostrophe
  mimeType <- takeTill isApostrophe
  skip isApostrophe
    >> skip isComma
    >> skipSpace
    >> string "size"
    >> skipSpace
  size <- decimal
  skipSpace
    >> string "bytes"
    >> skip isComma
    >> skipSpace
    >> string "description"
    >> skipSpace
    >> skip isApostrophe
  description <- takeTill isApostrophe
  skip isApostrophe
    >> skip isComma
    >> skipSpace
    >> string "file"
    >> skipSpace
    >> string "name"
    >> skipSpace
    >> skip isApostrophe
  fileName <- takeTill isApostrophe
  return $ VideoAttachment id' mimeType size description fileName

videoChapterParser :: Parser VideoChapter
videoChapterParser = do
  string "Chapters" >> skip isColon >> skipSpace
  entry <- digitToInt <$> digit
  skipSpace >> string "entries" >> return (VideoChapter entry)

videoTrackTagParser :: Parser VideoTrackTag
videoTrackTagParser = do
  string "Tags"
    >> skipSpace
    >> string "for"
    >> skipSpace
    >> string "track"
    >> skipSpace
    >> string "ID"
    >> skipSpace
  id' <- digitToInt <$> digit
  skip isColon >> skipSpace
  entry <- digitToInt <$> digit
  skipSpace >> string "entries" >> return (VideoTrackTag id' entry)

trackKindParser :: Parser TrackKind
trackKindParser
  =   (string "video" >> return Video)
  <|> (string "audio" >> return Audio)
  <|> (string "subtitles" >> return Subtitles)

isApostrophe :: Char -> Bool
isApostrophe = (== '\'')

isColon :: Char -> Bool
isColon = (== ':')

isComma :: Char -> Bool
isComma = (== ',')

isOpenBracket :: Char -> Bool
isOpenBracket = (== '(')

isCloseBracket :: Char -> Bool
isCloseBracket = (== ')')

toMaybe :: Either a b -> Maybe b
toMaybe = either (const Nothing) Just

exampleTargetFolder :: String
exampleTargetFolder = "/home/faris/Videos/Anime/Black Clover/Qwe"
