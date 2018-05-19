-- | Benchmark.Suite wrapper

module Benchmark.Suite
  ( Suite
  , copy
  , thawST
  , freezeST
  , runST
  , pureST
  , mutate
  ) where

import Prelude

import Benchmark.Suite.ST (STSuite)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Effect (Effect)

foreign import data Suite :: Type
foreign import _copy :: forall a b h. a -> ST h b

-- | Copy a mutable Suite
copy :: forall h. STSuite h -> ST h (STSuite h)
copy = _copy

-- | Convert an immutable Suite to a mutable Suite
thawST :: forall h. Suite -> ST h (STSuite h)
thawST = _copy

-- | Convert a mutable Suite to an immutable Suite
freezeST :: forall h. STSuite h -> ST h Suite
freezeST = _copy

-- | Freeze a mutable Suite object, creating an immutable object. Use this
-- | function as you would use `Prelude.runST` to freeze a mutable reference.
-- |
-- | The rank-2 type prevents the map from escaping the scope of `runST`.
foreign import runST :: (forall h. ST h (STSuite h)) -> Effect Suite

pureST :: (forall s. ST s (STSuite s)) -> Suite
pureST f = ST.run (f >>= freezeST)

mutate :: forall b. (forall s. STSuite s -> ST s b)
  -> Suite
  -> Suite
mutate f suiteST = pureST do
  s <- thawST suiteST
  _ <- f s
  pure s
