module BenchLib
  ( Size
  , Bench
  , Group
  , Suite
  , BenchResult
  , GroupResult
  , SuiteResult
  , SampleResult
  , CheckResults
  , RunOpts
  , SuiteOpts
  , GroupOpts
  , BenchOptsPure
  , BenchOptsM
  , Reporter
  , suite
  , group
  , bench
  , benchM
  , run_
  , suite_
  , group_
  , benchM_
  , bench_
  , basic
  , normalize
  , normalizeInput
  , normalizeOutput
  , checkAllEq
  , class MonadBench
  , toAff
  , class CanRunOnly
  , only
  , defaultReporter
  , reportConsole
  , run
  -- , basic
  ) where

import Prelude

import Data.Array (filter, foldr)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.DateTime.Instant (unInstant)
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Number.Format as NumFmt
import Data.String as Str
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable1 (replicate1A)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Effect.Ref as Ref
import Prim.TypeError (class Warn, Text)
import Unsafe.Coerce (unsafeCoerce)

--- Type Aliases

type Size = Int

--- Option Types

-- | Options to run benchmark suites.
type RunOpts =
  { reporters :: Array Reporter
  }

-- | Options for the benchmark suite.
type SuiteOpts =
  { sizes :: Array Size
  , iterations :: Int
  }

-- | Options for the benchmark group.
type GroupOpts a b =
  { sizes :: Array Int
  , iterations :: Int
  , checkInputs :: Maybe ({ results :: Array a, size :: Size } -> Boolean)
  , checkOutputs :: Maybe ({ results :: Array b, size :: Size } -> Boolean)
  }

-- | Options for pure benchmarks.
type BenchOptsPure a =
  { iterations :: Int
  , prepareInput :: Size -> a
  }

-- | Options for monadic benchmarks.
type BenchOptsM (m :: Type -> Type) a =
  { iterations :: Int
  , prepareInput :: Size -> m a
  }

--- Result Types

-- | The result of a benchmark suite.
type SuiteResult =
  { groupResults :: Array GroupResult
  , suiteName :: String
  }

-- | The result of a benchmark group.
type GroupResult =
  { groupName :: String
  , benchResults :: Array BenchResult
  , checkOutputsResults :: Maybe (Array CheckResults)
  , checkInputsResults :: Maybe (Array CheckResults)
  }

-- | The result of a benchmark.
type BenchResult =
  { benchName :: String
  , samples :: Array SampleResult
  }

-- | The result of one benchmark sample.
type SampleResult =
  { iterations :: Int
  , size :: Size
  , average :: Milliseconds
  }

--- Opaque types

-- | Opaque type for the benchmark suite.
newtype Suite = Suite
  { suiteName :: String
  , runSuite ::
      { iterations :: Int
      , reporter :: Reporter
      , sizes :: Array Size
      }
      -> Aff SuiteResult
  }

-- | Opaque type for the benchmark group.
newtype Group = Group
  { only :: Boolean
  , groupName :: String
  , runGroup ::
      { reporter :: Reporter
      , iterations :: Int
      , sizes :: Array Int
      }
      -> Aff GroupResult
  }

-- | Opaque type for the benchmark.
newtype Bench a b = Bench
  { only :: Boolean
  , benchName :: String
  , runBench ::
      { iterations :: Int
      , size :: Size
      }
      -> Aff { sampleResult :: SampleResult, input :: a, output :: b }
  }

derive instance Functor (Bench a)

--- Reporter types

-- | A reporter is a set of functions that are called at different stages of the benchmark.
-- | It allows to customize the output of the benchmark suite or perform other actions like writing to a file.
type Reporter =
  { onSuiteStart :: String -> Aff Unit
  , onGroupStart :: String -> Aff Unit
  , onBenchStart :: String -> Aff Unit
  , onSampleStart :: Size -> Aff Unit
  , onSampleFinish :: SampleResult -> Aff Unit
  , onBenchFinish :: BenchResult -> Aff Unit
  , onGroupFinish :: GroupResult -> Aff Unit
  , onSuiteFinish :: SuiteResult -> Aff Unit
  }

type CheckResults =
  { success :: Boolean
  , size :: Size
  , results :: Array { showedVal :: String, benchName :: String }
  }

--- Internal Types

type BenchName = String

type GroupName = String

--- Running the benchmark suite

-- | Run the benchmark suite.
run :: (RunOpts -> RunOpts) -> Suite -> Effect Unit
run mkRunOpts (Suite { runSuite }) = launchAff_ do
  let
    { reporters } = mkRunOpts defaultRunOpts

  -- Merge reporters into a single reporter:
  let
    reporter = foldr (<>) defaultReporter reporters

  let { iterations, sizes } = defaultSuiteOpts

  _suiteResult <- runSuite { reporter, iterations, sizes }

  pure unit

--- Exported utility functions

-- | Check if all elements in the array are equal.
-- | Useful for checking the results of benchmarks.
checkAllEq :: forall a. Eq a => Array a -> Boolean
checkAllEq items =
  case Array.head items of
    Just x -> Array.all (_ == x) items
    Nothing -> true

--- Defaults

defaultRunOpts :: RunOpts
defaultRunOpts =
  { reporters: [ reportConsole ]
  }

defaultSuiteOpts :: SuiteOpts
defaultSuiteOpts =
  { sizes: [ 0, 25_000, 50_000, 100_000 ]
  , iterations: 1000
  }

mkDefaultGroupOpts :: forall r a b. { sizes :: Array Size, iterations :: Int | r } -> GroupOpts a b
mkDefaultGroupOpts { sizes, iterations } =
  { sizes
  , iterations
  , checkInputs: Nothing
  , checkOutputs: Nothing
  }

mkDefaultBenchOptsPure :: forall r. { iterations :: Int | r } -> BenchOptsPure Size
mkDefaultBenchOptsPure { iterations } =
  { iterations
  , prepareInput: \size -> size -- TODO: mk effectful
  }

benchOptsPureToM :: forall m a. Applicative m => BenchOptsPure a -> BenchOptsM m a
benchOptsPureToM { iterations, prepareInput } =
  { iterations
  , prepareInput: prepareInput >>> pure
  }

--- Core functions

-- | Create a benchmark suite of a given name.
-- | The suite will be run with the provided options.
-- | The suite is a collection of groups, each containing multiple benchmarks.
suite :: String -> (SuiteOpts -> SuiteOpts) -> Array Group -> Suite
suite suiteName mkSuiteOpts groups_ = Suite
  { suiteName
  , runSuite: \{ reporter } -> do
      reporter.onSuiteStart suiteName

      let { iterations, sizes } = mkSuiteOpts defaultSuiteOpts
      let groups = mayGetOnlies groups_

      groupResults <- for groups \(Group { runGroup }) ->
        ( do
            groupResult <- runGroup { reporter, iterations, sizes }

            pure groupResult
        )

      let suiteResult = { groupResults, suiteName }

      reporter.onSuiteFinish suiteResult
      pure suiteResult

  }

type PerSizeItf a b =
  { addEntry :: { size :: Size, benchName :: String, input :: a, output :: b } -> Aff Unit
  , getCheckOutputsResults :: Aff (Maybe (Array CheckResults))
  , getCheckInputsResults :: Aff (Maybe (Array CheckResults))
  }

mkPerSizeItf :: forall a b. Show a => Show b => GroupOpts a b -> Aff (PerSizeItf a b)
mkPerSizeItf groupOpts = liftEffect do
  refAccum <- Ref.new (Map.empty :: Map Size (ResultPerSize a b))

  pure
    { addEntry: \{ size, benchName, input, output } -> liftEffect $ Ref.modify_
        ( Map.insertWith (<>) size
            { inputs: [ { benchName, value: input } ]
            , outputs: [ { benchName, value: output } ]
            }
        )
        refAccum

    , getCheckOutputsResults: do
        accum <- liftEffect $ Ref.read refAccum

        pure do
          checkOutputs <- groupOpts.checkOutputs
          Just $ map
            ( \(size /\ { outputs }) ->
                { size
                , success: checkOutputs { results: map _.value outputs, size }
                , results: map (\{ benchName, value } -> { benchName, showedVal: show value }) outputs
                }
            )
            (Map.toUnfoldable accum)

    , getCheckInputsResults: do
        accum <- liftEffect $ Ref.read refAccum

        pure do
          checkInputs <- groupOpts.checkInputs
          Just $ map
            ( \(size /\ { inputs }) ->
                { size
                , success: checkInputs { results: map _.value inputs, size }
                , results: map (\{ benchName, value } -> { benchName, showedVal: show value }) inputs
                }
            )
            (Map.toUnfoldable accum)
    }

-- | Create a benchmark group of a given name.
-- | The group will be run with the provided options.
-- | The group is a collection of benchmarks
group :: forall @a @b. Show a => Show b => String -> (GroupOpts a b -> GroupOpts a b) -> Array (Bench a b) -> Group
group groupName mkGroupOpts benches_ =
  Group
    { groupName
    , only: false
    , runGroup: \defOpts@{ reporter } -> do
        reporter.onGroupStart groupName

        let
          groupOpts@{ sizes, iterations } = mkGroupOpts $ mkDefaultGroupOpts defOpts

        let benches = mayGetOnlies benches_

        perSizeItf <- mkPerSizeItf groupOpts

        benchResults <- for benches
          ( \(Bench { benchName, runBench }) -> do
              reporter.onBenchStart benchName
              samples <- for sizes
                ( \size -> do
                    reporter.onSampleStart size

                    { sampleResult, output, input } <- runBench { iterations, size }

                    perSizeItf.addEntry { size, benchName, output, input }

                    reporter.onSampleFinish sampleResult
                    pure sampleResult
                )

              let benchResult = { benchName, samples }

              reporter.onBenchFinish benchResult
              pure benchResult
          )

        checkOutputsResults <- perSizeItf.getCheckOutputsResults
        checkInputsResults <- perSizeItf.getCheckOutputsResults

        let groupResult = { groupName, benchResults, checkOutputsResults, checkInputsResults }

        reporter.onGroupFinish groupResult

        pure groupResult
    }

benchImpl :: forall m a b. MonadBench m => String -> (BenchOptsPure Size -> BenchOptsM m a) -> (a -> m b) -> Bench a b
benchImpl benchName mkBenchOpts benchFn = unsafeCoerce 1

-- Bench
--   { benchName
--   , only: false
--   , runBench: \defOpts@{ size } -> do

--       let { iterations, prepareInput } = mkBenchOpts $ mkDefaultBenchOptsPure defOpts

--       inputs :: NonEmptyArray _ <- replicate1A iterations (toAff $ prepareInput size)

--       let benchFnAff = toAff <<< benchFn

--       duration <- measureTime \_ -> for inputs \input -> benchFnAff input

--       let average = Milliseconds (unwrap duration / Int.toNumber iterations)

--       input <- toAff $ prepareInput size

--       output <- toAff $ benchFn input

--       let sampleResult = { size, average, iterations }

--       pure { sampleResult, output: pure output, input: pure input }
--   }

type ResultPerSize a b =
  { inputs :: Array { value :: a, benchName :: String }
  , outputs :: Array { value :: b, benchName :: String }
  }

-- Normalization

normalize :: forall a a' b b'. (a -> a') -> (b -> b') -> Bench a b -> Bench a' b'
normalize normIn normOut (Bench rec) = Bench $ rec
  { runBench = \opts -> do
      ret@{ input, output } <- rec.runBench opts
      pure $ ret { input = normIn input, output = normOut output }
  }

normalizeInput :: forall a a' b. (a -> a') -> Bench a b -> Bench a' b
normalizeInput norm = normalize norm identity

normalizeOutput :: forall a b b'. (b -> b') -> Bench a b -> Bench a b'
normalizeOutput norm = normalize identity norm

basic :: forall m a b. Bench a b -> Bench Unit Unit
basic = normalize (const unit) (const unit)

--- Typeclasses

-- | A combinator for entities that can be run only once.
-- | This is useful while debugging or developing benchmarks.
-- | So you do not have to run the whole suite again and again.
class CanRunOnly a where
  only :: Warn (Text "`only` usage") => a -> a

instance CanRunOnly (Bench a b) where
  only (Bench rec) = Bench $ rec { only = true }

instance CanRunOnly Group where
  only (Group rec) = Group $ rec { only = true }

class IsOnly a where
  isOnly :: a -> Boolean

instance IsOnly (Bench a b) where
  isOnly (Bench rec) = rec.only

instance IsOnly Group where
  isOnly (Group rec) = rec.only

-- | A class for monadic benchmarks.
-- | It allows to run benchmarks in different monads as long as they are capable of
-- | getting turned into an Aff monad.
class Monad m <= MonadBench m where
  toAff :: forall a. m a -> Aff a

instance MonadBench Effect where
  toAff = liftEffect

instance MonadBench Aff where
  toAff = identity

instance MonadBench Identity where
  toAff (Identity val) = pure val

--- API shortcuts

bench :: forall @a @b. String -> (BenchOptsPure Size -> BenchOptsPure a) -> (a -> b) -> Bench a b
bench name mkOpts benchFn = benchImpl name mkOpts' (pure <<< benchFn)
  where
  mkOpts' :: BenchOptsPure Size -> BenchOptsM Identity a
  mkOpts' optsPure = benchOptsPureToM $ mkOpts optsPure

bench_ :: forall b. String -> (Size -> b) -> Bench Size b
bench_ name benchFn = bench name identity benchFn

benchM :: forall m a b. MonadBench m => String -> (BenchOptsM m Size -> BenchOptsM m a) -> (a -> m b) -> Bench a b
benchM name mkOpts benchFn = benchImpl name mkOpts' benchFn
  where
  mkOpts' :: BenchOptsPure Size -> BenchOptsM m a
  mkOpts' optsPure = mkOpts $ benchOptsPureToM optsPure

-- | Like `benchM`, but with default options.
benchM_ :: forall m b. MonadBench m => String -> (Size -> m b) -> Bench Size b
benchM_ name benchFn = benchM name identity benchFn

run_ :: Suite -> Effect Unit
run_ = run identity

-- | Like `suite`, but with default options.
suite_ :: String -> Array Group -> Suite
suite_ groupName benchmarks = suite groupName identity benchmarks

-- | Like `group`, but with default options.
group_ :: forall a b. Show a => Show b => String -> Array (Bench a b) -> Group
group_ groupName benches = group groupName identity benches

--- Utils

mayGetOnlies :: forall a. IsOnly a => Array a -> Array a
mayGetOnlies mayOnlies =
  let
    onlys = filter isOnly mayOnlies
  in
    if Array.null onlys then mayOnlies else onlys

measureTime :: forall a m. MonadEffect m => (Unit -> m a) -> m Milliseconds
measureTime action = do
  startTime <- liftEffect now
  _ <- action unit
  endTime <- liftEffect now
  let duration = unwrap (unInstant endTime) - unwrap (unInstant startTime)
  pure (Milliseconds duration)

asciColorStr :: Int -> String -> String
asciColorStr color str =
  let
    colorCode = "\x1b[" <> show color <> "m"
    resetCode = "\x1b[0m"
  in
    colorCode <> str <> resetCode

bgGray :: Int
bgGray = 100

-- Reporter

-- | Default reporter useful for selective overriding.
-- | It will do nothing.
defaultReporter :: Reporter
defaultReporter =
  { onSuiteStart: const $ pure unit
  , onGroupStart: const $ pure unit
  , onSampleStart: const $ pure unit
  , onBenchStart: const $ pure unit
  , onSuiteFinish: const $ pure unit
  , onGroupFinish: const $ pure unit
  , onSampleFinish: const $ pure unit
  , onBenchFinish: const $ pure unit
  }

-- | Console reporter.
-- | It will print the results to the console in human readable format.
reportConsole :: Reporter
reportConsole = defaultReporter
  { onSuiteStart = \name -> Console.log
      ("• suite: " <> name)

  , onGroupStart = \name -> Console.log
      ("  • group: " <> name)

  , onBenchStart = \benchName -> Console.log
      ("    • " <> (asciColorStr bgGray ("bench: " <> benchName)))

  , onSampleFinish = \{ size, average, iterations } -> Console.log
      ( "      • " <> printStats
          [ "size" /\ Int.toStringAs Int.decimal size
          , "count" /\ Int.toStringAs Int.decimal iterations
          , "avg" /\ printMs average
          ]
      )

  , onSuiteFinish = \_ -> Console.log "Suite finished"
  }

printStats :: Array (String /\ String) -> String
printStats stats = Str.joinWith ", " (map (\(k /\ v) -> k <> "=" <> v) stats)

printMs :: Milliseconds -> String
printMs (Milliseconds ms) = NumFmt.toStringWith (NumFmt.fixed 4) ms <> "ms"