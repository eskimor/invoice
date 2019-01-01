{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AccountingConf where

import           Data.ByteString
import           Data.Decimal
import           Data.Text (Text)
import           System.FilePath (FilePath, (</>))

data AccountingConf = AccountingConf
  { _accountingPath   :: FilePath -- ^ Where you keep your accounting file
  , _accountingIncome :: Text -- ^ The account used for income.
  , _accountingClaims :: Text -- ^ The account where you record your claims.
  }
