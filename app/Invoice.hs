{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Data.Decimal
import           Data.Default       (def)
import           Data.List          (nub)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Monoid
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Data.Time
import           Hledger.Data.Types
import           Hledger.Read       (readJournalFile)
import           System.Directory   (doesFileExist, removeFile, renameFile,
                                     setCurrentDirectory)
import           System.Exit        (ExitCode (ExitFailure), exitWith)
import           System.FilePath    (replaceExtension, takeFileName, (</>))
import           System.Process     (system)

import           Config
import           Entry
import           Invoice
import           TimeCamp

main :: IO ()
main = do
  setCurrentDirectory baseDir
  preparedData <- readTimeClockFile
  let invoiceText = renderInvoice preparedData

  template <- T.readFile invoiceTemplate
  (today, invoiceNr) <- getDateStuff
  let processed =
        T.replace "$invoice-nr$" invoiceNr
        . T.replace "$invoice$" invoiceText
        . T.replace "$date$" today
        $ template

  let invoiceName = billedPath </> T.unpack invoiceNr <> "-obsidian.tex"
  let pdfName = replaceExtension invoiceName "pdf"
  T.writeFile invoiceName processed
  void $ system $ "xelatex " <> invoiceName
  renameFile timeClockFile (billedPath </> T.unpack invoiceNr <> ".timeclock")
  renameFile (takeFileName pdfName) pdfName -- Latex puts pdf in working directory ...
  -- Cleanup:
  forM_ ["aux", "log"] $ \ext ->
    removeFile (takeFileName $ replaceExtension invoiceName ext)

  -- TimeCamp:
  doTimeCamp . concat . Map.elems $ preparedData
  -- Show what we did:
  void $ system $ "evince " <> pdfName
