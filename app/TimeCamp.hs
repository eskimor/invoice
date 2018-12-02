{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.Map           as Map

import Entry
import Invoice
import TimeCamp

main :: IO ()
main = do
  preparedData <- readTimeClockFile
  doTimeCamp . concat . Map.elems $ preparedData
