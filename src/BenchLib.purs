-- | A benchmark, slightly simplified, can be seen as this three step transformation:
-- |
-- | 1. `prepare :: size -> input`
-- | 2. `benchmark :: input -> return`
-- | 3. `finalize :: return -> output`
-- |
-- | Common type variables:
-- |   - `inp`: type of input for benchmark, can be diifferent for each benchmark in a group. It is created by the `prepare` function.
-- |   - `ret`: type of result of benchmark, can be diifferent for each benchmark in a group.
-- |   - `out`: type of output of the benchmark, must be equal for all benchmarks in a group. Created by the `finalize` function. 
-- |     Having this extra type variable e.g. allows to check if benchmark results of a group do return values considered equal.
-- |   - `m`: the monad in which the benchmark is run. It must be an instance of `MonadBench`. Instances are provided for `Effect` and `Aff`. 

module BenchLib
  ( Bench
  , Group
  , Suite
  , BenchResult
  , SampleResult
  , GroupResults
  , SuiteResults
  , CheckResults
  , Reporter
  , Size
  , benchSuite
  , benchGroup
  , bench
  , benchM
  , benchSuite_
  , benchGroup_
  , bench_
  , benchM_
  , checkEq
  , class MonadBench
  , toAff
  , class CanRunOnly
  , only
  , defaultReporter
  , reportConsole
  , run
  , eval
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (filter)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Exists (Exists)
import Data.Int as Int
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Number.Format as NumFmt
import Data.String as Str
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, for_, sum)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicate)
import Data.Unfoldable1 (replicate1A)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Effect.Ref as Ref
import Prim.TypeError (class Warn, Text)
import Safe.Coerce (coerce)

--- Type Aliases

type Size = Int

--- Option Types

-- | Options for the benchmark suite.
type SuiteOpts =
  { sizes :: Array Size
  , iterations :: Int
  , reporters :: Array Reporter
  }

-- | Options for the benchmark group.
type GroupOpts out =
  { overrideSizes :: Array Int
  , overrideIterations :: Int
  , overrideReporters :: Array Reporter
  , checkOutputs :: Maybe ({ outputs :: Array out, size :: Size } -> Boolean)
  }

-- | Options for monadic benchmarks.
type BenchOptsM (m :: Type -> Type) input result output =
  { overrideIterations :: Int
  , overrideReporters :: Array Reporter
  , prepare :: Size -> m input
  , finalize :: result -> m output
  }

-- | Options for pure benchmarks.
type BenchOptsPure input result output =
  { overrideIterations :: Int
  , overrideReporters :: Array Reporter
  , prepare :: Size -> input
  , finalize :: result -> output
  }

--- Result Types

-- | The result of a benchmark suite.
type SuiteResults =
  { suiteName :: String
  , groups :: Array GroupResults
  }

-- | The result of a benchmark group.
type GroupResults =
  { groupName :: String
  , benchs :: Array BenchResult
  }

-- | The result of a benchmark.
type BenchResult =
  { benchName :: String
  , samples :: Array SampleResult
  }

type SampleResult =
  { size :: Size
  , duration :: Milliseconds
  , iterations :: Int
  }

--- Opaque types

-- | Opaque type for the benchmark suite.
newtype Suite = Suite
  { suiteName :: String
  , run :: SuiteOpts -> Aff SuiteResults
  }

-- | Opaque type for the benchmark group.
newtype Group = Group
  ( MayOnly
      { groupName :: String
      , run :: GroupOpts Unit -> Aff GroupResults
      }
  )

-- | Opaque type for the benchmark.
newtype Bench out = Bench
  ( MayOnly
      { benchName :: String
      , run :: BenchOptsPure Size out out -> Size -> Aff (SampleResult /\ out)
      }
  )

--- Reporter types

-- | A reporter is a set of functions that are called at different stages of the benchmark.
-- | It allows to customize the output of the benchmark suite or perform other actions like writing to a file.
type Reporter =
  { onSuiteStart :: String -> Effect Unit
  , onGroupStart :: String -> Effect Unit
  , onSizeStart :: Size -> Effect Unit
  , onBenchStart :: String -> Effect Unit
  , onSuiteFinish :: SuiteResults -> Effect Unit
  , onGroupFinish :: GroupResults -> Effect Unit
  , onSizeFinish :: SampleResult -> Effect Unit
  , onBenchFinish :: SampleResult -> Effect Unit
  , onCheckResults :: CheckResults -> Effect Unit
  }

type CheckResults =
  { success :: Boolean
  , size :: Size
  , outputs :: Array { strOutput :: String, benchName :: String }
  }

--- Internal Types

type BenchName = String

type GroupName = String

type MayOnly a = { only :: Boolean, val :: a }

--- Running the benchmark suite

-- | Run the benchmark suite.
run :: Suite -> Effect Unit
run suite = launchAff_ $ void $ eval suite

-- | Evaluate the benchmark suite.
eval :: Suite -> Aff SuiteResults
eval (Suite suite) = suite.run defaultSuiteOpts

--- Exported utility functions

-- | Check if all elements in the array are equal.
-- | Useful for checking the results of benchmarks.
checkEq :: forall a. Eq a => Array a -> Boolean
checkEq items =
  let
    first = Array.head items

    checkEq' :: a -> Boolean
    checkEq' x = Array.all (_ == x) items
  in
    case first of
      Just x -> checkEq' x
      Nothing -> true

--- Defaults

defaultSuiteOpts :: SuiteOpts
defaultSuiteOpts =
  { sizes: [ 0, 25_000, 50_000, 100_000 ]
  , iterations: 1000
  , reporters: [ reportConsole ]
  }

mkDefaultGroupOpts :: forall a. SuiteOpts -> GroupOpts a
mkDefaultGroupOpts { sizes, iterations, reporters } =
  { overrideSizes: sizes
  , overrideIterations: iterations
  , overrideReporters: reporters
  , checkOutputs: Nothing
  }

mkDefaultBenchOpts :: forall out. GroupOpts out -> BenchOptsPure Size out out
mkDefaultBenchOpts { overrideIterations, overrideReporters } =
  { overrideIterations
  , overrideReporters
  , prepare: identity
  , finalize: identity
  }

--- Core functions

-- | Create a benchmark suite of a given name.
-- | The suite will be run with the provided options.
-- | The suite is a collection of groups, each containing multiple benchmarks.
benchSuite :: String -> (SuiteOpts -> SuiteOpts) -> Array Group -> Suite
benchSuite suiteName mkOpts groups_ = Suite
  { suiteName
  , run: \_ -> do
      let opts = mkOpts defaultSuiteOpts

      let groups = mayGetOnlies (map (\(Group g) -> g) groups_)

      runReporters opts.reporters \rep -> rep.onSuiteStart suiteName

      groupResults <- for groups \group -> do
        group.run (mkDefaultGroupOpts opts)

      let results = { suiteName, groups: groupResults }

      runReporters opts.reporters \rep -> rep.onSuiteFinish results

      pure results
  }

type Itf out =
  { add :: Size -> { benchName :: String, output :: out } -> Effect Unit
  , get :: Effect (Array (ResultPerSize out))
  }

f :: forall out. Effect (Itf out)
f = do
  refOutputs <- Ref.new (Map.empty :: Map Size (ResultPerSize out))

  pure
    { add: \size out -> Ref.modify_
        ( Map.alter
            ( case _ of
                Nothing -> Just
                  { size
                  , outputs: [ out ]
                  }
                Just val -> Just val { outputs = val.outputs <> [ out ] }
            )
            size
        )
        refOutputs

    , get: do
        outputs <- Ref.read refOutputs
        pure $ List.toUnfoldable $ Map.values outputs
    }

-- | Create a benchmark group of a given name.
-- | The group will be run with the provided options.
-- | The group is a collection of benchmarks
benchGroup :: forall @out. Eq out => Show out => String -> (GroupOpts Unit -> GroupOpts out) -> Array (Bench out) -> Group
benchGroup groupName mkOpts benches_ = Group $ notOnly
  { groupName
  , run: \defOpts -> do

      let groupOpts = mkOpts defOpts

      runReporters groupOpts.overrideReporters \rep -> rep.onGroupStart groupName

      let benches = mayGetOnlies $ map (\(Bench b) -> b) benches_

      refOutputs <- liftEffect $ Ref.new (Map.empty :: Map Size (ResultPerSize out))

      itf <- liftEffect f

      results <- for benches \bench -> do
        runReporters groupOpts.overrideReporters \rep -> rep.onBenchStart bench.benchName
        r <- for groupOpts.overrideSizes \size -> do
          runReporters groupOpts.overrideReporters \rep -> rep.onSizeStart size

          let benchOpts = mkDefaultBenchOpts groupOpts

          { duration, iterations } /\ output <- bench.run benchOpts size

          runReporters groupOpts.overrideReporters \rep -> rep.onSizeFinish { size, duration, iterations }

          liftEffect $ itf.add size { benchName: bench.benchName, output }

          pure ({ size, duration, iterations } /\ output)

        pure { benchName: bench.benchName, samples: map fst r }

      let groupResults = { groupName, benchs: results }

      outs <- liftEffect itf.get
      for_ outs $ checkResults groupOpts

      runReporters groupOpts.overrideReporters \rep -> rep.onGroupFinish groupResults

      pure groupResults
  }

data Foo out = Foo { check :: GroupOpts out -> ResultPerSize out -> Aff Unit, results :: ResultPerSize out }

benchImpl :: forall m inp ret out. Eq out => MonadBench m => String -> (BenchOptsPure Size out out -> BenchOptsM m inp ret out) -> (inp -> m ret) -> Bench out
benchImpl benchName mkOpts benchFn = Bench $ notOnly
  { benchName
  , run: \defOpts size -> do

      let opts@{ overrideIterations: iterations } = mkOpts defOpts

      durs :: NonEmptyList _ <- replicate1A iterations do

        input :: inp <- toAff $ opts.prepare size

        (_ /\ duration) <- measureTime \_ -> do
          toAff $ benchFn input

        pure duration

      let duration = calcMean durs

      let
        sampleResult =
          { iterations
          , duration
          , size
          }

      output <- toAff do
        input :: inp <- opts.prepare size
        result <- benchFn input
        opts.finalize result

      pure (sampleResult /\ output)
  }

type ResultPerSize out = { outputs :: Array { output :: out, benchName :: String }, size :: Size }

checkResults :: forall out. Show out => Eq out => GroupOpts out -> ResultPerSize out -> Aff Unit
checkResults groupOpts all@{ size } =
  for_ groupOpts.checkOutputs \check -> do

    if check { size, outputs: map _.output all.outputs } then do
      runReporters groupOpts.overrideReporters \rep -> rep.onCheckResults
        { success: true
        , size
        , outputs
        }
    else do
      runReporters groupOpts.overrideReporters \rep -> rep.onCheckResults
        { success: false
        , size
        , outputs
        }
      throwError (error "Benchmarks results are not equal")
  where
  outputs = map (\val -> { strOutput: show val.output, benchName: val.benchName }) all.outputs

--- Typeclasses

-- | A combinator for entities that can be run only once.
-- | This is useful while debugging or developing benchmarks.
-- | So you do not have to run the whole suite again and again.
class CanRunOnly a where
  only :: Warn (Text "`only` usage") => a -> a

instance CanRunOnly (Bench a) where
  only (Bench mayOnly) = Bench (mayOnly { only = true })

instance CanRunOnly Group where
  only (Group mayOnly) = Group (mayOnly { only = true })

-- | A class for monadic benchmarks.
-- | It allows to run benchmarks in different monads as long as they are capable of
-- | getting turned into an Aff monad.
class Monad m <= MonadBench m where
  toAff :: forall a. m a -> Aff a

instance MonadBench Effect where
  toAff = liftEffect

instance MonadBench Aff where
  toAff = identity

--- API shortcuts

-- | Create a benchmark of a given name.
-- | The benchmark will be run with the provided options.
-- | The benchmark is a function that takes an input and returns an output.
-- | The input is generated by the prepare function, and the output is processed by the finalize function.
bench :: forall inp ret out. Eq out => String -> (BenchOptsPure Size out out -> BenchOptsPure inp ret out) -> (inp -> ret) -> Bench out
bench name mkOpts benchFn = benchImpl name (benchOptsPureToAff @Effect <<< mkOpts) (pure <<< benchFn)

-- | Like `bench`, but with default options.
bench_ :: String -> (Size -> Unit) -> Bench Unit
bench_ name benchFn = bench name identity (benchFn >>> const unit)

-- | Like `bench``, but with a monadic function.
benchM :: forall m inp ret out. MonadBench m => Eq out => String -> (BenchOptsM m Size out out -> BenchOptsM m inp ret out) -> (inp -> m ret) -> Bench out
benchM name mkOpts benchFn = benchImpl name (mkOpts <<< benchOptsPureToAff) benchFn

-- | Like `benchM`, but with default options.
benchM_ :: forall @m out. Eq out => MonadBench m => String -> (Size -> m out) -> Bench Unit
benchM_ name benchFn = benchM name identity (benchFn >>> map (const unit))

-- | Like `benchSuite`, but with default options.
benchSuite_ :: String -> Array Group -> Suite
benchSuite_ groupName benchmarks = benchSuite groupName identity benchmarks

-- | Like `benchGroup`, but with default options.
benchGroup_ :: String -> Array (Bench Unit) -> Group
benchGroup_ groupName benches = benchGroup groupName identity benches

---

--- Utils

runReporters :: Array Reporter -> (Reporter -> Effect Unit) -> Aff Unit
runReporters reporters f = liftEffect $ for_ reporters f

benchOptsPureToAff :: forall @m inp ret out. Applicative m => BenchOptsPure inp ret out -> BenchOptsM m inp ret out
benchOptsPureToAff { overrideIterations, overrideReporters, prepare, finalize } =
  { overrideIterations
  , overrideReporters
  , prepare: \size -> pure $ prepare size
  , finalize: \result -> pure $ finalize result
  }

mayGetOnlies :: forall a. Array (MayOnly a) -> Array a
mayGetOnlies mayOnlies =
  let
    onlys = filter _.only mayOnlies
  in
    map _.val $ if Array.null onlys then mayOnlies else onlys

notOnly :: forall a. a -> MayOnly a
notOnly a = { only: false, val: a }

measureTime :: forall a m. MonadEffect m => (Unit -> m a) -> m (a /\ Milliseconds)
measureTime action = do
  startTime <- liftEffect now
  result <- action unit
  endTime <- liftEffect now
  let duration = unwrap (unInstant endTime) - unwrap (unInstant startTime)
  pure (result /\ Milliseconds duration)

calcMean :: NonEmptyList Milliseconds -> Milliseconds
calcMean items = Milliseconds (sum (map coerce items :: NonEmptyList Number) / Int.toNumber (NEL.length items))

asciColorStr :: Int -> String -> String
asciColorStr color str =
  let
    colorCode = "\x1b[" <> show color <> "m"
    resetCode = "\x1b[0m"
  in
    colorCode <> str <> resetCode

bgGray :: Int
bgGray = 100

memoizeEffect :: forall a b. Ord a => (a -> b) -> Effect (a -> Effect b)
memoizeEffect f = do
  cacheRef <- Ref.new Map.empty
  pure \x -> do
    cache <- Ref.read cacheRef
    case Map.lookup x cache of
      Just result -> pure result
      Nothing -> do
        let result = f x
        Ref.modify_ (Map.insert x result) cacheRef
        pure result

-- Reporter

-- | Default reporter useful for selective overriding.
-- | It will do nothing.
defaultReporter :: Reporter
defaultReporter =
  { onSuiteStart: const $ pure unit
  , onGroupStart: const $ pure unit
  , onSizeStart: const $ pure unit
  , onBenchStart: const $ pure unit
  , onSuiteFinish: const $ pure unit
  , onGroupFinish: const $ pure unit
  , onSizeFinish: const $ pure unit
  , onBenchFinish: const $ pure unit
  , onCheckResults: const $ pure unit
  }

-- | Console reporter.
-- | It will print the results to the console in human readable format.
reportConsole :: Reporter
reportConsole = defaultReporter
  { onSuiteStart = \name -> Console.log
      ("• suite: " <> name)

  , onGroupStart = \name -> Console.log
      ("  • group: " <> name)

  , onSizeFinish = \{ size, duration, iterations } -> Console.log
      ( "      • " <> printStats
          [ "size" /\ Int.toStringAs Int.decimal size
          , "avg" /\ printMs duration
          , "count" /\ Int.toStringAs Int.decimal iterations
          ]
      )

  , onBenchStart = \benchName -> Console.log
      ("    • " <> (asciColorStr bgGray ("bench: " <> benchName)))

  -- , onBenchFinish = \{ duration: dur, iterations } -> Console.log
  --     ("        • mean duration: " <> printMs dur <> " ms (" <> show iterations <> " iterations)\n")

  , onCheckResults = \{ success, size, outputs } ->
      if success then Console.log
        ("    • check: ✓")
      else do
        Console.log
          ("    • check: ✗")
        for_ outputs \{ benchName, strOutput } -> Console.log
          ("      • " <> benchName <> ": " <> show size)
  }

printStats :: Array (String /\ String) -> String
printStats stats = Str.joinWith ", " (map (\(k /\ v) -> k <> "=" <> v) stats)

printMs :: Milliseconds -> String
printMs (Milliseconds ms) = NumFmt.toStringWith (NumFmt.fixed 4) ms <> "ms"

padLeft :: Int -> String -> String
padLeft n str =
  let
    len = Str.length str
    pad = Str.joinWith "" $ replicate (n - len) " "
  in
    pad <> str

padRight :: Int -> String -> String
padRight n str =
  let
    len = Str.length str
    pad = Str.joinWith "" $ replicate (n - len) " "
  in
    str <> pad