{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Invoice where

import           Data.Decimal
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Data.Time
import           Hledger.Data.Types

import           Entry
import           Config
import           AccountingConf

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

sumEntries :: [Entry] -> Decimal
sumEntries = sum . map entryAmount

writeAccountingBilledEntry :: (Text, Text) -> [Entry] -> AccountingConf -> IO ()
writeAccountingBilledEntry n es cfg = do
  let appendAccounting = T.appendFile (_accountingPath cfg) . ("\n" <>)
  appendAccounting $ makeAccountingBilledEntry n es cfg

makeAccountingBilledEntry :: (Text, Text) -> [Entry] -> AccountingConf -> Text
makeAccountingBilledEntry (invDate, invNum) es cfg =
  let
    s = sumEntries es
  in
    T.unlines
    [ invDate <> "   " <> "Invoice Nr. " <> invNum
    , "  " <> _accountingIncome cfg <> "  -" <> renderAmount (rate * s) <> " EUR"
    , "  " <> _accountingClaims cfg
    ]

renderEntry :: Entry -> Text
renderEntry entr =
  let
    renderedDate = T.pack $ formatTime defaultTimeLocale "%04Y/%m/%d" (entryDate entr)

    renderedRate = T.pack . show $ rate

    renderedAmount = renderAmount $ entryAmount entr

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
