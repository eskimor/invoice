{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Parsing and prepartion of `Entry`.
--
--   For invoicing and reporting to `TimeCamp`

module Entry where

import           Control.Lens
import           Control.Monad
import           Data.Decimal
import           Data.Default       (def)
import           Data.List          (nub)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time
import           Hledger.Data.Types
import           Hledger.Read       (readJournalFile)
import           System.Directory   (doesFileExist)
import           System.Exit        (ExitCode (ExitFailure), exitWith)

import Config


data Entry
  = Entry { entryDate        :: Day
          , entryProject     :: AccountName
          , entryDescription :: [Text]
          , entryAmount      :: Decimal
          } deriving (Eq, Ord, Show)

prettyPrintEntry :: Entry -> Text
prettyPrintEntry e@(Entry d p _ am) = T.unlines
    [ "Date: " <> T.pack (show d)
    , "Project: " <> p
    , "Description: " <> prettyPrintDescription e
    , "Amount: " <> T.pack (show am)
    ]

prettyPrintEntries :: [Entry] -> Text
prettyPrintEntries = T.intercalate "--------------------\n" . map prettyPrintEntry

-- | Read time clock file if it exists and `prepareData`.
readTimeClockFile :: IO (Map AccountName [Entry])
readTimeClockFile = do
  somethingTodo <- doesFileExist timeClockFile

  unless somethingTodo $ do
    putStrLn "Nothing to do, no time clocked yet!"
    exitWith (ExitFailure 1)

  Right journal <- readJournalFile def timeClockFile
  pure $ prepareData journal

toEntry :: Transaction -> Entry
toEntry trans =
  let
    posting = head . tpostings $ trans
    entryDate = tdate trans
    entryProject = paccount posting
    entryDescription = [tdescription trans]
    entryAmount = aquantity . unmixed . pamount $ posting

    unmixed (Mixed (a:[])) = a
    unmixed (Mixed [])     = error "Mixed amount was empty!"
    unmixed _              = error "Mixed amount had more than one entry!"
  in
    Entry {..}


-- | Extract data from journal and prepare for invoice.
--
--   The returned data is a `Entry` list for each `AccountName`, with one
--   `Entry` per day. (Multiple entries per day are merged.)
prepareData :: Journal -> Map AccountName [Entry]
prepareData = fmap mergeDayEntries . splitProjects . getEntries
  where
    getEntries = map toEntry . jtxns



prettyPrintDescription :: Entry -> Text
prettyPrintDescription  = T.intercalate "; " . nub . entryDescription

renderAmount :: Decimal -> Text
renderAmount = T.pack . show . roundTo 2


-- | Merge entries on a single day into a single "Entry".
--
--   This function assumes that entries are ordered and belong all to the same
--   project.
mergeDayEntries :: [Entry] -> [Entry]
mergeDayEntries = foldr mergeEntries []
  where
    mergeEntries x [] = [x]
    mergeEntries x os@(o:tailOs) =
      let
        addEntry :: Entry -> Entry
        addEntry old = old { entryDescription = entryDescription x
                                             <> entryDescription old
                           , entryAmount = entryAmount old + entryAmount x
                           }

        isNewDay = entryDate o /= entryDate x
      in
        if isNewDay
           then x:os
           else addEntry o : tailOs

splitProjects :: [Entry] -> Map AccountName [Entry]
splitProjects = foldr addEntry Map.empty
  where
    addEntry x = at (entryProject x) . non [] %~ (x:)

