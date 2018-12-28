module Counter
    ( Counter
    , empty
    , insert
    ) where

import Data.Hashable
import qualified Data.HashMap.Strict as HM

type Counter k = HM.HashMap k Int

empty :: Counter k
empty = HM.empty

insert :: (Hashable k, Eq k) => k -> Counter k -> Counter k
insert k = HM.insertWith (+) k 1
