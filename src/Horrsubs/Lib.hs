module Horrsubs.Lib
  ( extractSubtitles
  ) where

import           Control.Applicative
import qualified Control.Exception as EX
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Bool
import           Data.Char
import           Data.Maybe
import qualified Data.Text as T
import           System.Directory
import           System.FilePath
import           System.Process

data TargetEntity = TargetFile FilePath
                  | TargetDirectory [TargetEntity]
                  deriving (Show)

extractSubtitles :: FilePath -> IO ()
extractSubtitles path = do
  targetEntity <- makeTargetEntity path
  case targetEntity of
    Nothing -> return ()
    Just targetEntity' -> do
      videoProps <- makeVideoProperties targetEntity'
      mapM_ extractSubtitlesImpl videoProps

makeTargetEntity :: FilePath -> IO (Maybe TargetEntity)
makeTargetEntity path = do
  isFileExist <- doesFileExist path
  if isFileExist && "mkv" `isExtensionOf` path
    then return $ Just $ TargetFile path
    else do
    isDirectoryExist <- doesDirectoryExist path
    if isDirectoryExist
      then do
      subPaths <- fmap (combine path) . concat . toMaybe <$> fetchedPaths
      targetEntities <- catMaybes <$> mapM makeTargetEntity subPaths
      return $ Just $ TargetDirectory targetEntities
      else return Nothing
  where
    fetchedPaths :: IO (Either EX.SomeException [FilePath])
    fetchedPaths = EX.try $ listDirectory path

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
  , videoTrackKind :: VideoTrackKind
  , videoTrackFormat :: T.Text
  } deriving (Show)

newtype VideoTrackId = VideoTrackId
  { unVideoTrackId :: Int
  } deriving (Show)

data VideoTrackKind = Video | Audio | Subtitles deriving (Show)

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

makeVideoProperties :: TargetEntity -> IO [VideoProperty]
makeVideoProperties (TargetFile filePath) = do
  mkvInfo <- fmap T.pack . toMaybe <$> infoString
  videoPropInfos <- return $ concat $ parseMkvInfo =<< mkvInfo
  return $ [VideoProperty filePath videoPropInfos]
  where
    parseMkvInfo :: T.Text -> Maybe [VideoInfo]
    parseMkvInfo a =
      let parser = many1 $ videoInfoParser <* endOfLine
      in toMaybe $ parseOnly parser a

    infoString :: IO (Either EX.SomeException String)
    infoString = EX.try $ readProcess programPath ["-i", filePath] ""

    programPath :: FilePath
    programPath = "/home/faris/.nix-profile/bin/mkvmerge"
makeVideoProperties (TargetDirectory entities) =
  concat <$> mapM makeVideoProperties entities

extractSubtitlesImpl :: VideoProperty -> IO ()
extractSubtitlesImpl videoProp = mapM_ extract'
  $ zipWith (,) [0..]
  $ catMaybes
  $ fmap asSubtitlesVideoTrack
  $ videoPropertyInfos videoProp
  where
    extract' :: (Integer, VideoTrack) -> IO ()
    extract' (idx, videoTrack) =
      let ext = bool (show idx <> ".srt") "srt" $ idx == 0
          srtPath = replaceExtension filePath ext
      in extractSubtitlesImpl' filePath srtPath videoTrack

    filePath :: FilePath
    filePath = videoPropertyPath videoProp

    asSubtitlesVideoTrack :: VideoInfo -> Maybe VideoTrack
    asSubtitlesVideoTrack (VideoInfoTrack videoTrack) =
      case videoTrackKind videoTrack of
        Subtitles -> Just videoTrack
        _         -> Nothing
    asSubtitlesVideoTrack _ = Nothing

extractSubtitlesImpl' :: FilePath -> FilePath -> VideoTrack -> IO ()
extractSubtitlesImpl' filePath srtPath videoTrack =
  case videoTrackKind videoTrack of
    Subtitles -> bool (void extract') (return ()) =<< doesFileExist srtPath
    _         -> return ()
  where
    extract' :: IO (Either EX.SomeException ())
    extract' = do
      putStrLn $ "FILE PATH: " <> filePath
      putStrLn $ "SRT PATH: " <> srtPath
      EX.try $ callProcess programPath
        [ "tracks"
        , filePath
        , show trackId <> ":" <> srtPath
        ]

    trackId :: Int
    trackId = unVideoTrackId $ videoTrackId videoTrack

    programPath :: FilePath
    programPath = "/home/faris/.nix-profile/bin/mkvextract"

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
  kind <- videoTrackKindParser
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

videoTrackKindParser :: Parser VideoTrackKind
videoTrackKindParser
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
