{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Invoice where

import           Data.Decimal
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time
import           Hledger.Data.Types

import           Entry
import           Config

renderInvoice :: Map AccountName [Entry] -> Text
renderInvoice prjs =
  let
   renderProjectTitle n = "\\ProjectTitle{Project " <> n <> "}"

   renderItem (n, entries) = renderProjectTitle n <> "\n"
                          <> T.unlines (map renderEntry entries)
  in
    "\\begin{invoice}{EUR}{0}\n"
    <> T.unlines (map renderItem (Map.toList prjs))
    <> "\\end{invoice}\n"

renderEntry :: Entry -> Text
renderEntry entr =
  let
    renderedDate = T.pack $ formatTime defaultTimeLocale "%04Y/%m/%d" (entryDate entr)

    renderedRate = T.pack . show $ rate

    renderedAmount = T.pack . show . roundTo 2 $ entryAmount entr

    renderedDescription = escapeLatexStuff . prettyPrintDescription $ entr

    escapeLatexStuff = T.replace "#" "\\#"
  in
    "\\Fee{" <> renderedDate <> ": " <> renderedDescription <> "}{" <> renderedRate <> "}" <> "{" <> renderedAmount <> "}"

-- Get date and invoice number:
getDateStuff :: IO (Text, Text)
getDateStuff = do
  t <- getCurrentTime
  z <- getCurrentTimeZone
  pure $ ( T.pack $ formatTime defaultTimeLocale "%04Y/%m/%d" (utcToLocalTime z t)
         , T.pack $ formatTime defaultTimeLocale "%04Y-%m-%d" (utcToLocalTime z t)
         )

