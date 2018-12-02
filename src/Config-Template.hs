{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Config where

import           Data.ByteString
import           Decimal
import           System.FilePath (FilePath, (</>))

-- | All data is exepected to reside in here.
--
--   Executables will chdir into this directory before beginning execution.
baseDir :: FilePath
baseDir = "/where/you/keep/your/timeclock/file"

-- | Where we are going to put our invoices.
billedPath :: FilePath
billedPath = baseDir </> "billed/"

-- | The file we clock in/out to.
timeClockFile :: FilePath
timeClockFile = baseDir </> "work.timeclock"

-- | Latex template file we are going to use.
invoiceTemplate :: FilePath
invoiceTemplate = baseDir </> "invoice-template.tex"

-- | Your hourly rate.
rate :: Decimal
rate = error "Your hourly rate goes here"

-- Auth Token for timecamp integrationg.
timeCampAuthToken :: ByteString
timeCampAuthToken = error "Your time camp auth token, if you want to use the timecamp integration."
