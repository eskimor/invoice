-- #!/usr/bin/env nix-shell
-- #!nix-shell -i runghc -p haskellPackages.ghcWithPackages(haskellPackages:[haskellPackages.text])
{-# LANGUAGE OverloadedStrings #-}

module ClockInOut where

import           Control.Monad       (void)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Data.Time.Clock
import           Data.Time.Format    (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime
import           System.Environment
import           System.Process      (system)

import Config

roundToQuarter :: UTCTime -> UTCTime
roundToQuarter t = addUTCTime (fromIntegral rounded) (t { utctDayTime = 0 })
  where
    rounded :: Int
    rounded = round (utctDayTime t/(60*15)) * 60 * 15

data Mode = ClockIn  | ClockOut

progNameToMode :: String ->  Mode
progNameToMode "ti" = ClockIn
progNameToMode "to" = ClockOut
progNameToMode _    = error "Program must be called as eigher ti or to."

getPrefix :: Mode -> Text
getPrefix ClockIn  = "i "
getPrefix ClockOut = "o "


addIfNonEmpty :: Text -> Text -> Text
addIfNonEmpty s t = if T.null t then "" else s <> t

addSpaceIfNonEmpty :: Text -> Text
addSpaceIfNonEmpty = addIfNonEmpty " "

main :: IO ()
main = do
  t <- roundToQuarter <$> getCurrentTime
  z <- getCurrentTimeZone
  desc <- T.unwords . map T.pack <$> getArgs
  mode <- progNameToMode <$> getProgName
  let
    timeStamp = T.pack $ formatTime defaultTimeLocale "%04Y/%m/%d %H:%M:%S%z" (utcToLocalTime z t)
    prefix = getPrefix mode
  case mode of
    ClockIn  -> do
      T.appendFile timeClockFile $ prefix <> timeStamp <> addSpaceIfNonEmpty desc
      T.putStrLn $ "Clocked in at: " <> timeStamp
    ClockOut -> do
      -- Put description to "i" line:
      T.appendFile timeClockFile $ addIfNonEmpty "  " desc <> "\n"
      T.appendFile timeClockFile $ prefix <> timeStamp <> "\n"
      T.putStrLn $ "Clocked out at: " <> timeStamp
      T.putStrLn "Today:"
      void . system $ "hledger -f " <> timeClockFile <> " balance -D -ptoday"
      T.putStrLn "\n--------------------\n"
      T.putStrLn "Weekly:"
      void . system $ "hledger -f " <> timeClockFile <> " balance -D -pweekly"


