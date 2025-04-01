module BenchLib
  ( Suite
  , Group
  , Bench

  , SuiteResults
  , GroupResults
  , BenchResult

  , Reporter
  , Size

  , benchSuite_
  , benchSuite

  , benchGroup_
  , benchGroup

  , bench_
  , bench

  , benchM_
  , benchM

  , checkEq

  , class MonadBench
  , toAff

  , eval
  , run

  , reportConsole
  , defaultReporter
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (filter)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Int as Int
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, for_, sum)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable1 (replicate1A)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Effect.Ref as Ref
import Record as R
import Safe.Coerce (coerce)

type Size = Int

type BenchName = String

type GroupName = String

type MayOnly a = { only :: Boolean, val :: a }

--- Opts

-- | Options for the benchmark suite.
type SuiteOpts =
  { sizes :: Array Size
  , iterations :: Int
  , reporters :: Array Reporter
  }

-- | Options for the benchmark group.
type GroupOpts =
  { sizes :: Array Int
  , iterations :: Int
  , check :: Maybe (forall a. Eq a => Array a -> Boolean)
  , reporters :: Array Reporter
  }

-- | Options for monadic benchmarks.
type BenchOptsM (m :: Type -> Type) input result output =
  { iterations :: Int
  , prepare :: Size -> m input
  , finalize :: result -> m output
  , reporters :: Array Reporter
  }

-- | Options for pure benchmarks.
type BenchOptsPure input result output =
  { iterations :: Int
  , prepare :: Size -> input
  , finalize :: result -> output
  , reporters :: Array Reporter
  }

--- Results

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
  , size :: Size
  , duration :: Milliseconds
  , iterations :: Int
  }

---

-- | Opaque type for the benchmark suite.
newtype Suite = Suite
  { suiteName :: String
  , run :: SuiteOpts -> Aff SuiteResults
  }

-- | Opaque type for the benchmark group.
newtype Group = Group
  ( MayOnly
      { groupName :: String
      , run :: GroupOpts -> Aff GroupResults
      }
  )

-- | Opaque type for the benchmark.
newtype Bench out = Bench
  ( MayOnly
      { benchName :: String
      , run :: BenchOptsPure Unit out out -> Size -> Aff (BenchResult /\ out)
      }
  )

---

type Reporter =
  { onSuiteStart :: String -> Effect Unit
  , onGroupStart :: String -> Effect Unit
  , onSizeStart :: Size -> Effect Unit
  , onBenchStart :: { benchName :: String, size :: Size } -> Effect Unit
  , onSuiteFinish :: SuiteResults -> Effect Unit
  , onGroupFinish :: GroupResults -> Effect Unit
  , onSizeFinish :: Size -> Effect Unit
  , onBenchFinish :: BenchResult -> Effect Unit
  , onCheckResults :: { success :: Boolean, results :: Array { benchName :: String, output :: String } } -> Effect Unit
  }


-- | Run the benchmark suite.
run :: Suite -> Effect Unit
run suite = launchAff_ $ void $ eval suite

-- | Evaluate the benchmark suite.
eval :: Suite -> Aff SuiteResults
eval (Suite suite) = suite.run defaultSuiteOpts

---

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
  { sizes: [ 0, 10, 100 ]
  , iterations: 1000
  , reporters: [ reportConsole ]
  }

mkDefaultGroupOpts :: SuiteOpts -> GroupOpts
mkDefaultGroupOpts { sizes, iterations, reporters } =
  { sizes
  , iterations
  , check: Nothing
  , reporters
  }

mkDefaultBenchOpts :: forall c. GroupOpts -> BenchOptsPure Unit c c
mkDefaultBenchOpts { iterations, reporters } =
  { iterations
  , prepare: \_ -> unit
  , finalize: identity
  , reporters
  }

--- Bench functions

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

-- | Create a benchmark group of a given name.
-- | The group will be run with the provided options.
-- | The group is a collection of benchmarks
benchGroup :: forall @a. Eq a => Show a => String -> (GroupOpts -> GroupOpts) -> Array (Bench a) -> Group
benchGroup groupName mkOpts benches_ = Group $ notOnly
  { groupName
  , run: \defOpts -> do

      let groupOpts = mkOpts defOpts

      runReporters groupOpts.reporters \rep -> rep.onGroupStart groupName

      let benches = mayGetOnlies $ map (\(Bench b) -> b) benches_

      results <- for groupOpts.sizes
        ( \size -> do

            runReporters groupOpts.reporters \rep -> rep.onSizeStart size

            resultsPerBench <- for benches
              ( \{ benchName, run } -> do
                  let benchOpts = mkDefaultBenchOpts groupOpts
                  { duration, iterations } /\ output <- run benchOpts size

                  pure ({ benchName, duration, iterations } /\ output)
              )

            checkResults groupOpts (map (\({ benchName } /\ output) -> { benchName, output }) resultsPerBench)

            runReporters groupOpts.reporters \rep -> rep.onSizeFinish size

            pure (map (\(r /\ _) -> R.merge r { size }) resultsPerBench)
        )

      let groupResults = { groupName, benchs: join results }

      runReporters groupOpts.reporters \rep -> rep.onGroupFinish groupResults

      pure groupResults
  }

benchImpl :: forall m a b c. Eq c => MonadBench m => String -> (BenchOptsPure Unit c c -> BenchOptsM m a b c) -> (a -> m b) -> Bench c
benchImpl benchName mkOpts benchFn = Bench $ notOnly
  { benchName
  , run: \defOpts size -> do

      let opts@{ iterations } = mkOpts defOpts

      runReporters opts.reporters \rep -> rep.onBenchStart { benchName, size }

      durs :: NonEmptyList _ <- replicate1A iterations do

        input :: a <- toAff $ opts.prepare size

        (_ /\ duration) <- measureTime \_ -> do
          toAff $ benchFn input

        pure duration

      let duration = calcMean durs

      let
        benchResult =
          { iterations
          , duration
          , size
          , benchName
          }

      runReporters opts.reporters \rep -> rep.onBenchFinish benchResult

      output <- toAff do
        input :: a <- opts.prepare size
        result <- benchFn input
        opts.finalize result

      pure (benchResult /\ output)
  }

--- MonadBench

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
bench :: forall a b c. Eq c => String -> (BenchOptsPure Unit c c -> BenchOptsPure a b c) -> (a -> b) -> Bench c
bench name mkOpts benchFn = benchImpl name (benchOptsPureToAff @Effect <<< mkOpts) (pure <<< benchFn)

-- | Like `bench`, but with default options.
bench_ :: forall c. Eq c => String -> (Unit -> c) -> Bench c
bench_ name benchFn = bench name identity benchFn

-- | Like `bench``, but with a monadic function.
benchM :: forall m a b c. MonadBench m => Eq c => String -> (BenchOptsM m Unit c c -> BenchOptsM m a b c) -> (a -> m b) -> Bench c
benchM name mkOpts benchFn = benchImpl name (mkOpts <<< benchOptsPureToAff) benchFn

-- | Like `benchM`, but with default options.
benchM_ :: forall @m c. Eq c => MonadBench m => String -> (Unit -> m c) -> Bench c
benchM_ name benchFn = benchM name identity benchFn

-- | Like `benchSuite`, but with default options.
benchSuite_ :: String -> Array Group -> Suite
benchSuite_ groupName benchmarks = benchSuite groupName identity benchmarks

-- | Like `benchGroup`, but with default options.
benchGroup_ :: forall @a. Eq a => Show a => String -> Array (Bench a) -> Group
benchGroup_ groupName benches = benchGroup groupName identity benches

---

checkResults :: forall r a. Show a => Eq a => GroupOpts -> Array { benchName :: String, output :: a | r } -> Aff Unit
checkResults groupOpts results_ =
  for_ groupOpts.check \check -> do

    if (check (map _.output results_)) then do
      runReporters groupOpts.reporters \rep -> rep.onCheckResults
        { success: true
        , results
        }
    else do
      runReporters groupOpts.reporters \rep -> rep.onCheckResults
        { success: false
        , results
        }
      throwError (error "Benchmarks results are not equal")
  where
  results = map (\({ benchName, output }) -> { benchName, output: show output }) results_

--- Utils

runReporters :: Array Reporter -> (Reporter -> Effect Unit) -> Aff Unit
runReporters reporters f = liftEffect $ for_ reporters f

benchOptsPureToAff :: forall @m a b c. Applicative m => BenchOptsPure a b c -> BenchOptsM m a b c
benchOptsPureToAff { iterations, prepare, finalize, reporters } =
  { iterations
  , prepare: \size -> pure $ prepare size
  , finalize: \result -> pure $ finalize result
  , reporters
  }

mayGetOnlies :: forall a. Array (MayOnly a) -> Array a
mayGetOnlies mayOnlies =
  let
    onlys = filter _.only mayOnlies
  in
    map _.val $ if Array.null onlys then mayOnlies else onlys

only :: forall a. MayOnly a -> MayOnly a
only { val } = { val, only: true }

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

reportConsole :: Reporter
reportConsole = defaultReporter
  { onSuiteStart = \name -> Console.log
      ("• suite: " <> name)

  , onGroupStart = \name -> Console.log
      ("  • group: " <> name)

  , onSizeStart = \size -> Console.log
      ("    • size: " <> show size)

  , onBenchStart = \{ benchName, size } -> Console.log
      ("      • " <> (asciColorStr bgGray ("bench: " <> benchName <> " (size = " <> show size <> ")")))

  , onBenchFinish = \{ duration: Milliseconds dur, iterations } -> Console.log
      ("        • mean duration: " <> show dur <> " ms (" <> show iterations <> " iterations)\n")

  , onCheckResults = \{ success, results } ->
      if success then Console.log
        ("      • check: ✓")
      else do
        Console.log
          ("       • check: ✗")
        for_ results \{ benchName, output } -> Console.log
          ("          • " <> benchName <> ": " <> output)
  }

