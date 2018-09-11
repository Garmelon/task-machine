module Main where

import           Control.Monad

import qualified Brick          as B

import           TaskMachine.UI

main :: IO()
main = void $ B.defaultMain myApp startState
