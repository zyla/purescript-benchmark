-- | Contains functions that operate on `Suite`, which is an immutable
-- | representation of the Benchmark.js' suite object.
-- |
-- | Currently, there are no plans to add functions that modify the immutable
-- | `Suite` as that would require cloning the Suite on each operation.
-- | For this reason, typically you want to use `SuiteM` and
-- | `Benchmark.Suite.Monad`, which provide a monadic interface to a mutable
-- | representation of the suite object (`SuiteST s`).

module Benchmark.Suite.Immutable where

import Benchmark.Suite (Suite)
import Effect (Effect)
import Prelude (Unit)

-- | Executes all benchmarks within the suite.
foreign import runSuite :: Suite -> Effect Unit
