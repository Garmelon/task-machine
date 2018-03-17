module Main where

import qualified Database.SQLite.Simple as DB

import qualified TaskMachine.Database as TMB

main :: IO ()
main = DB.withConnection "test.db" TMB.initializeNewDB
