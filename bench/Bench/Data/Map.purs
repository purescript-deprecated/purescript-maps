module Bench.Data.Map where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (foldl)
import Data.List (zipWith)
import Data.List as L
import Data.Map as M
import Data.Tuple (Tuple(..))
import Performance.Minibench (bench, benchWith)

benchMap :: Eff (console :: CONSOLE) Unit
benchMap = do
  log "size"
  log "---------------"
  benchSize

  log ""

  log "fromFoldable"
  log "------------"
  benchFromFoldable

  log ""

  log "foldl"
  log "------------"
  benchFoldl

  where

  nats = L.range 0 999999
  natPairs = zipWith Tuple nats nats
  singletonMap = M.singleton 0 0
  smallMap = M.fromFoldable $ L.take 100 natPairs
  midMap = M.fromFoldable $ L.take 10000 natPairs
  bigMap = M.fromFoldable $ natPairs

  benchSize = do

    log "size: singleton map"
    bench \_ -> M.size singletonMap

    log $ "size: small map (" <> show (M.size smallMap) <> ")"
    bench \_ -> M.size smallMap

    log $ "size: midsize map (" <> show (M.size midMap) <> ")"
    benchWith 100 \_ -> M.size midMap

    log $ "size: big map (" <> show (M.size bigMap) <> ")"
    benchWith 10  \_ -> M.size bigMap

  benchFromFoldable = do
    let natStrs = show <$> L.range 0 99999
        natPairs = (flip Tuple) unit <$> natStrs
        shortPairList = L.take 10000 natPairs

    log $ "fromFoldable (" <> show (L.length shortPairList) <> ")"
    benchWith 100 \_ -> M.fromFoldable shortPairList

    log $ "fromFoldable (" <> show (L.length natPairs) <> ")"
    benchWith 10 \_ -> M.fromFoldable natPairs

  benchFoldl = do
    let sum = foldl (+) 0

    log "foldl: singleton map"
    bench \_ -> sum singletonMap

    log $ "foldl: small map (" <> show (M.size smallMap) <> ")"
    bench \_ -> sum smallMap

    log $ "foldl: midsize map (" <> show (M.size midMap) <> ")"
    benchWith 100 \_ -> sum midMap

    log $ "foldl: big map (" <> show (M.size bigMap) <> ")"
    benchWith 10  \_ -> sum bigMap

