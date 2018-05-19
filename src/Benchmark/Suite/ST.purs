module Benchmark.Suite.ST where

import Prelude (Unit)
import Effect (Effect)
import Control.Monad.ST (ST, kind Region)

foreign import data STSuite :: Region -> Type
foreign import data BenchmarkEvent :: Type

-- API
--------------------

foreign import new :: forall s. ST s (STSuite s)

foreign import add :: forall s a.  STSuite s -> String -> Effect a -> ST s Unit

foreign import on :: forall h.
  STSuite h -> String -> (BenchmarkEvent -> Effect Unit) -> ST h Unit

-- Extra
--------------------

type BenchmarkResult =
  { name :: String
  , hz :: Number
  , stats :: {
      rme :: Number
    }
  }

foreign import accumulateResults :: forall s.
  STSuite s -> (Array BenchmarkResult -> Effect Unit) -> ST s Unit
