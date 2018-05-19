module Benchmark.Suite.Monad
  (
  -- Types
    SuiteM
  , class MonadSuite
  , liftSuiteST
  , SuiteT
  -- General
  , runSuiteT
  -- API Wrappers
  , add
  , on
  , accumulateResults
  , runSuiteM
  ) where

import Prelude

import Benchmark.Event (toString, BenchmarkEventName)
import Benchmark.Suite (Suite, pureST)
import Benchmark.Suite.Immutable as Immutable
import Benchmark.Suite.ST (STSuite, BenchmarkResult)
import Benchmark.Suite.ST as STS
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (lift)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

-- Types
--------------------

type SuiteM s m a = MonadSuite s m => a

newtype SuiteT s a = SuiteT (ReaderT (STSuite s) (ST s) a)

class (Monad m, MonadReader (STSuite s) m) <= MonadSuite s m | m -> s where
  liftSuiteST :: forall a. ST s a -> m a

-- Generalized Newtype Deriving Instances
--------------------

derive instance newtypeT :: Newtype (SuiteT s a) _
derive newtype instance functorSuiteT :: Functor (SuiteT s)
derive newtype instance applySuiteT :: Apply (SuiteT s)
derive newtype instance applicativeSuiteT :: Applicative (SuiteT s)
derive newtype instance bindSuiteT :: Bind (SuiteT s)
derive newtype instance monadSuiteT :: Monad (SuiteT s)

derive newtype instance monadAskSuiteT :: MonadAsk (STSuite s) (SuiteT s)
derive newtype instance monadReaderSuiteT :: MonadReader (STSuite s) (SuiteT s)

instance monadSuiteSuiteT :: MonadSuite s (SuiteT s) where
  liftSuiteST = SuiteT <<< lift

-- General
--------------------

runSuiteT :: forall s a. SuiteT s a -> Suite
runSuiteT (SuiteT m) = pureST do
  s <- STS.new
  let eff = (runReaderT m) (unsafeCoerce s)
  _ <- unsafeCoerce $ eff
  pure s

-- | Runs SuiteM transformer stack. This is equal to executing `suite.run()`,
-- | where suite is constructed via the monad interface:
-- | >>> runSuiteM $ do
-- | >>>   add "functionOne" myFunction
-- | >>>   add "functionTwo" myFunctionTwo
-- |
-- | The code above will construct a suite with two functions to benchmark and
-- | run those benchmarks.
runSuiteM :: forall s a.  SuiteT s a -> Effect Unit
runSuiteM m = Immutable.runSuite $ runSuiteT m

-- Internal helpers
--------------------

-- | Converts an `f` with two argumens (first accepting `STSuite s`) into an `f`
-- | that uses SuiteM
asksSTSuiteA2 :: forall s m a2 b.
     (STSuite s -> a2 -> ST s b)
  -> (SuiteM s m (a2 -> m b))
asksSTSuiteA2 fA2 a2 = do
  s <- ask
  liftSuiteST $ fA2 s a2

-- | Converts an `f` with three arguments (first accepting `STSuite s`) into an i
-- | `f` that uses SuiteM
asksSTSuiteA3 :: forall s m a2 a3 b.
     (STSuite s -> a2 -> a3 -> ST s b)
  -> (SuiteM s m (a2 -> a3 -> m b))
asksSTSuiteA3 fA3 a2 a3 = do
  s <- ask
  liftSuiteST $ fA3 s a2 a3

-- Suite API wrappers
--------------------

-- | Adds a test to the benchmark suite. Takes a name to identify the benchmark,
-- | and the test to benchmark.
add :: forall s m a. SuiteM s m (String -> Effect a -> m Unit)
add = asksSTSuiteA3 STS.add

-- | Registers a listener for the specified event type(s).
on :: forall s m.
  SuiteM s m (BenchmarkEventName -> (STS.BenchmarkEvent -> Effect Unit) -> m Unit)
on evName cb = do
  s <- ask
  liftSuiteST $ STS.on s (toString evName) cb

-- | Accumulates results of each cycle in an array. `onComplete` calls the
-- | provided callback with the array containing accumulated results.
accumulateResults :: forall s m.
  SuiteM s m ((Array BenchmarkResult -> Effect Unit) -> m Unit)
accumulateResults = asksSTSuiteA2 STS.accumulateResults
