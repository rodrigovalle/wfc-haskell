module Counter
    ( Counter
    , empty
    , insert
    , sample
    ) where

import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.List (find, mapAccumL)
import Data.Maybe
import System.Random

type Counter k = HM.HashMap k Int

empty :: Counter k
empty = HM.empty

insert :: (Hashable k, Eq k) => k -> Counter k -> Counter k
insert k = HM.insertWith (+) k 1

totalCount :: Counter k -> Int
totalCount = HM.foldr (+) 0

-- build an inverted index
invert :: Counter k -> [(Int, k)]
invert = snd . mapAccumL go 0 . HM.toList
  where
    go acc (k, v) =
        let nacc = acc + v
         in (nacc, (nacc, k))

sample :: Counter k -> IO k
sample counter = do
    rn <- randomRIO (0, totalCount counter)
    let sample = findSample rn (invert counter)
    return (snd sample)
  where
    findSample rand = fromJust . find ((< rand) . fst) . reverse
