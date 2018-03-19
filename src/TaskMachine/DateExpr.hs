{-# LANGUAGE OverloadedStrings #-}

module TaskMachine.DateExpr
  ( DateExpr
  , parse
  , save
  ) where

import Control.Exception

import qualified Data.Text as T
import qualified Database.SQLite.Simple           as DB
import qualified Database.SQLite.Simple.FromField as DB
import qualified Database.SQLite.Simple.Ok        as DB
import qualified Database.SQLite.Simple.ToField   as DB

data DateExpr = DummyValue

parse :: T.Text -> Maybe DateExpr
parse = const (Just DummyValue)

save :: DateExpr -> T.Text
save = const "dummy string"


data DummyException = DummyException
  deriving (Show)

instance Exception DummyException

instance DB.ToField DateExpr where
  toField = DB.SQLText . save

instance DB.FromField DateExpr where
  fromField f = case DB.fromField f of
    DB.Errors e -> DB.Errors e
    DB.Ok text -> case parse text of
      Nothing   -> DB.Errors [SomeException DummyException] -- TODO: Use proper exception
      Just expr -> DB.Ok expr
