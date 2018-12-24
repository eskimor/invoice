{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TimeCamp where

import           Control.Lens           hiding ((.=))
import           Control.Monad          (when)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Decimal
import           Data.Default.Class
import           Data.Foldable          (traverse_)
import qualified Data.HashMap.Lazy      as HMap
import           Data.List              (sort)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (mapMaybe)
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Data.Time
import           Data.Tuple             (swap)
import qualified Data.Vector            as V
import           Debug.Trace            (trace)
import           Network.HTTP.Req

import           Config
import           Entry


-- | TimeCamp task id.
import qualified Data.ByteString.Char8  as BS
type TaskId = Text

-- | Id of a TimeCamp entry.
type EntryId = Text

{-# NOINLINE authHeader #-}
authHeader :: Option scheme
authHeader = header "Authorization" timeCampAuthToken

makeTimeCampUri :: Text -> Url 'Https
makeTimeCampUri endpoint = https "www.timecamp.com" /: "third_party" /: "api" /: endpoint /: "format" /: "json"

timeCampTasks :: Url 'Https
timeCampTasks = makeTimeCampUri "tasks"

timeCampEntries :: Day -> Day -> Url 'Https
timeCampEntries fromL toL = makeTimeCampUri "entries" /: "from" /: fromRendered /: "to" /: toRendered /: "user_ids" /: "1273035"
  where
    fromRendered = renderDay fromL
    toRendered = renderDay toL

timeCampEntriesPostPut :: Url 'Https
timeCampEntriesPostPut = makeTimeCampUri "entries"



-- | Log given entries to TimeCamp:
--
--   CAUTION: Make sure that you don't have more than one entry per project and day.
--   Otherwise not all data will be logged. In other words it is assumed that
--   (entryDate, entryProject) uniquely identifies an entry.
doTimeCamp :: [Entry] -> IO ()
doTimeCamp entries = do
  ids <- getTaskIds
  let days = sort $ map entryDate entries
      firstEntryDay = listToMaybe days
      lastEntryDay = listToMaybe . reverse $ days
      mTimeRange = (,) <$> firstEntryDay <*> lastEntryDay
  timeCampEntriesL <- fromMaybe [] <$> traverse (uncurry getEntries) mTimeRange

  let
    (newEntries, updateEntries) = getNewOrUpdateEntries timeCampEntriesL entries
  when (not . null $ updateEntries) $ do
    putStrLn $ "INFO: " <> show (length updateEntries) <> " entries will be updated:"
    T.putStrLn $ prettyPrintEntries . map snd $ updateEntries
  when (not . null $ newEntries) $ do
    putStrLn $ "INFO: " <> show (length newEntries) <> " new entries will be created:"
    T.putStrLn $ prettyPrintEntries newEntries

  when (any (isNothing . flip Map.lookup ids . entryProject) entries) $ do
    putStrLn "WARNING: Some entries don't have a valid project name:"
    T.putStrLn . prettyPrintEntries . filter (isNothing . flip Map.lookup ids . entryProject) $ entries

  traverse_  (traverse postEntry . addId id ids) $ newEntries
  traverse_  (traverse putEntry  . addId snd ids) $ updateEntries

  pure ()
  where
    addId getEntry ids e = do
      id_ <- ids ^. at (entryProject . getEntry $ e)
      pure (e, id_)

-- | Find entries that need updates and the ones that are new.
--
--   If an entry on the same Day for the same project already exists on time
--   camp we assume it needs to be updated/replaced. If no such timecamp entry
--   exists the entry will be put in the first list without an id, so it will
--   be created on timecamp.
getNewOrUpdateEntries :: [(EntryId, Entry)] -> [Entry] -> ([Entry], [(EntryId, Entry)])
getNewOrUpdateEntries old entries = (Map.elems new, Map.elems updates)
  where
    new :: Map (Day, Text) Entry
    new = Map.difference entryMap oldMap

    updates :: Map (Day, Text) (EntryId, Entry)
    updates = Map.intersectionWith mergeGetId oldMap entryMap

    mergeGetId (eId, _) u = (eId, u)

    oldMap = Map.fromList . map (buildKeyVal snd) $ old
    entryMap = Map.fromList . map (buildKeyVal id) $ entries

    buildKeyVal :: (a -> Entry) -> a -> ((Day, Text), a)
    buildKeyVal getEntry e = ((entryDate . getEntry $ e, entryProject . getEntry $ e), e)


renderDay :: Day -> Text
renderDay = T.pack . formatTime defaultTimeLocale "%0Y-%m-%d"

postEntry :: (Entry, TaskId) -> IO ()
postEntry (e, tId) = runReq def $ do
  r <- req POST -- method
    timeCampEntriesPostPut
    reqBody
    ignoreResponse
    authHeader
  when (responseStatusCode r /= 200 && responseStatusCode r  /= 201) $ liftIO $ do
    T.putStrLn $ "Posting entry:\n" <> prettyPrintEntry e
    putStrLn $ "\nfailed with: " <> show (responseStatusCode r) <> show (responseStatusMessage r)
  where
    reqBody = mkPostPutReqBody Nothing e tId

putEntry :: ((EntryId, Entry), TaskId) -> IO ()
putEntry ((eId, e), tId) = runReq def $ do
  r <- req PUT -- method
    timeCampEntriesPostPut
    reqBody
    ignoreResponse
    authHeader
  when (responseStatusCode r /= 200 && responseStatusCode r  /= 201) $ liftIO $ do
    T.putStrLn $ "Updating entry:\n" <> prettyPrintEntry e
    putStrLn $ "\nfailed with: " <> show (responseStatusCode r) <> show (responseStatusMessage r)
  where
    reqBody = mkPostPutReqBody (Just eId) e tId

-- | Make a request body for POST/PUT entry requests.
--
--   For Post simply provide no `EntryId` (`Nothing`).
mkPostPutReqBody :: Maybe EntryId -> Entry -> TaskId -> ReqBodyUrlEnc
mkPostPutReqBody mEid e tId =
  ReqBodyUrlEnc $ "date" =: renderDay (entryDate e)
  <> "duration" =: (show . (* 3600) . roundTo 2 . entryAmount) e
  <> "note" =: prettyPrintDescription e
  <> "task_id" =: tId
  <> ( maybe mempty ("id" =:)) mEid
  <> "start_time" =: ("06:00:00" :: Text)
  <> "end_time" =: ("23:30:00" :: Text)

getTasks :: IO Value
-- You can either make your monad an instance of 'MonadHttp', or use
-- 'runReq' in any IO-enabled monad without defining new instances.
getTasks = runReq def $ do
  r <- req GET -- method
    timeCampTasks
    NoReqBody
    {- (ReqBodyJson payload) -- use built-in options or add your own -}
    jsonResponse -- specify how to interpret response
    authHeader

  {- let tasks = responseBody r -}
  {- liftIO $ print (responseBody r :: Value) -}
  pure $ responseBody r


getTaskIds :: IO (Map Text Text)
getTaskIds = do
  r <- getTasks
  pure $ Map.fromList . map swap . mapMaybe sequence . HMap.toList $ fmap (^? key "name" . _String) $ r ^. _Object


getEntries :: Day -> Day -> IO [(EntryId, Entry)]
getEntries fromDay toDay = do
  r <- runReq def $ do
    req GET -- method
      (timeCampEntries fromDay toDay)
      NoReqBody
      jsonResponse -- specify how to interpret response
      authHeader

  let rawEntries :: Value = responseBody r
  pure $ fromMaybe (error $ "Returned entries, were no Array: " <> show rawEntries) $ do
    entries <- rawEntries ^? _Array . to V.toList

    pure $ map parseEntry entries

  where
    parseEntry :: Value -> (EntryId, Entry)
    parseEntry obj = fromMaybe (error $ "parseEntry failed for: " <> show obj) $ do
      {- entryId <- obj ^? key "task_id" . _String . to Just -}
      entryDescription <- obj ^? key "description" . _String . to (:[])
      entryProject <- obj ^? key "name" . _String
      entryDateString <- obj ^? key "date" . _String
      entryDate <-  parseTimeM True defaultTimeLocale "%0Y-%m-%d" . T.unpack $ entryDateString
      duration <- obj ^? key "duration" . _String
      entryId <- obj ^? key "id" . _String
      let entryAmount = read (T.unpack duration) / 3600
      pure $ (entryId, Entry {..})

traceVal :: Show a => String -> a -> a
traceVal msg a = trace (msg ++ show a) a

