module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)
import Test.QuickCheck (QC)

import Test.Data.Map (mapTests)
import Test.Data.StrMap (strMapTests)

main :: forall eff. QC eff Unit
main = do
  log "Running Map tests"
  mapTests

  log "Running StrMap tests"
  strMapTests
