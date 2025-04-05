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
  ( SuiteOpts
  , Size
  --   Bench
  -- , Group
  -- , Suite
  -- , BenchResult
  -- , SampleResult
  -- , GroupResults
  -- , SuiteResults
  -- , CheckResults
  -- , Reporter
  -- , Size
  -- , benchSuite
  -- , benchGroup
  -- , bench
  -- , benchM
  -- , benchSuite_
  -- , benchGroup_
  -- , bench_
  -- , benchM_
  -- , checkEq
  -- , class MonadBench
  -- , toAff
  -- , class CanRunOnly
  -- , only
  -- , defaultReporter
  -- , reportConsole
  -- , run
  -- , eval
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (filter)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Int as Int
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Number.Format as NumFmt
import Data.Profunctor (class Profunctor, dimap)
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
import Unsafe.Coerce (unsafeCoerce)

--- Type Aliases

type Size = Int

--- Option Types

-- | Options for the benchmark suite.
type SuiteOpts =
  { sizes :: Array Size
  , iterations :: Int
  --, reporters :: Array Reporter
  }

-- | Options for the benchmark group.
type GroupOpts a b =
  { sizes :: Array Int
  , iterations :: Int
  , checkInputs :: Maybe ({ inputs :: Array a, size :: Size } -> Boolean)
  , checkOutputs :: Maybe ({ outputs :: Array b, size :: Size } -> Boolean)
  }

-- | Options for benchmarks.
type BenchOpts =
  { overrideIterations :: Int
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
  { duration :: Milliseconds
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
      , run ::
          { reporters :: Array Reporter
          , iterations :: Int
          , sizes :: Array Int
          , checkOutputs :: Maybe ({ outputs :: Array Unit, size :: Size } -> Boolean)
          }
          -> Aff GroupResults
      }
  )

-- | Opaque type for the benchmark.
newtype Bench a b = Bench
  ( MayOnly
      { benchName :: String
      , run :: { iterations :: Int, reporters :: Array Reporter } -> a -> Aff (SampleResult /\ b)
      }
  )

instance Profunctor Bench where
  dimap f g (Bench mo) = Bench $ map
    ( \rec -> rec
        { run = \opts inp -> map (map g) $ rec.run opts (f inp) }
    )
    mo

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

newtype MayOnly a = MayOnly { only :: Boolean, val :: a }

derive instance Functor MayOnly

-- --- Running the benchmark suite

-- -- | Run the benchmark suite.
-- run :: Suite -> Effect Unit
-- run suite = launchAff_ $ void $ eval suite

-- -- | Evaluate the benchmark suite.
-- eval :: Suite -> Aff SuiteResults
-- eval (Suite suite) = suite.run defaultSuiteOpts

-- --- Exported utility functions

-- -- | Check if all elements in the array are equal.
-- -- | Useful for checking the results of benchmarks.
-- checkEq :: forall a. Eq a => Array a -> Boolean
-- checkEq items =
--   let
--     first = Array.head items

--     checkEq' :: a -> Boolean
--     checkEq' x = Array.all (_ == x) items
--   in
--     case first of
--       Just x -> checkEq' x
--       Nothing -> true

-- --- Defaults

-- defaultSuiteOpts :: SuiteOpts
-- defaultSuiteOpts =
--   { sizes: [ 0, 25_000, 50_000, 100_000 ]
--   , iterations: 1000
--   , reporters: [ reportConsole ]
--   }

-- -- mkDefaultGroupOpts :: forall a. SuiteOpts -> GroupOpts a
-- -- mkDefaultGroupOpts { sizes, iterations } =
-- --   { sizes
-- --   , iterations
-- --   , checkOutputs: Nothing
-- --   }

-- mkDefaultBenchOpts :: forall ninp out. GroupOpts ninp out -> BenchOptsPure Size Size out out
-- mkDefaultBenchOpts { iterations } =
--   { overrideIterations: iterations
--   , prepare: identity
--   , finalize: identity
--   }

defaultBenchOpts :: BenchOpts
defaultBenchOpts =
  { overrideIterations: 0
  }

-- --- Core functions

-- -- | Create a benchmark suite of a given name.
-- -- | The suite will be run with the provided options.
-- -- | The suite is a collection of groups, each containing multiple benchmarks.
-- benchSuite :: String -> (SuiteOpts -> SuiteOpts) -> Array Group -> Suite
-- benchSuite suiteName mkOpts groups_ = Suite
--   { suiteName
--   , run: \_ -> do
--       let opts = mkOpts defaultSuiteOpts

--       let groups = mayGetOnlies (map (\(Group g) -> g) groups_)

--       runReporters opts.reporters \rep -> rep.onSuiteStart suiteName

--       groupResults <- for groups \group -> do
--         group.run
--           { reporters: opts.reporters
--           , iterations: opts.iterations
--           , sizes: opts.sizes
--           , checkOutputs: Nothing
--           }

--       let results = { suiteName, groups: groupResults }

--       runReporters opts.reporters \rep -> rep.onSuiteFinish results

--       pure results
--   }

-- type Itf out =
--   { add :: Size -> { benchName :: String, output :: out } -> Effect Unit
--   , get :: Effect (Array (ResultPerSize out))
--   }

-- f :: forall out. Effect (Itf out)
-- f = do
--   refOutputs <- Ref.new (Map.empty :: Map Size (ResultPerSize out))

--   pure
--     { add: \size out -> Ref.modify_
--         ( Map.alter
--             ( case _ of
--                 Nothing -> Just
--                   { size
--                   , outputs: [ out ]
--                   }
--                 Just val -> Just val { outputs = val.outputs <> [ out ] }
--             )
--             size
--         )
--         refOutputs

--     , get: do
--         outputs <- Ref.read refOutputs
--         pure $ List.toUnfoldable $ Map.values outputs
--     }

-- | Create a benchmark group of a given name.
-- | The group will be run with the provided options.
-- | The group is a collection of benchmarks
benchGroup :: forall a b. Eq b => Show b => String -> (GroupOpts a b -> GroupOpts a b) -> Array (Bench Size b) -> Group
benchGroup groupName mkOpts benches_ =  unsafeCoerce 1

--Group $ notOnly
--   { groupName
--   , run: \defOpts@{ reporters } -> do

--       let
--         groupOpts = mkOpts
--           { sizes: defOpts.sizes
--           , iterations: defOpts.iterations
--           , checkInputs: Nothing
--           , checkOutputs: Nothing
--           }

--       runReporters reporters \rep -> rep.onGroupStart groupName

--       let benches = mayGetOnlies $ map (\(Bench b) -> b) benches_

--       refOutputs <- liftEffect $ Ref.new (Map.empty :: Map Size (ResultPerSize out))

--       itf <- liftEffect f

--       results <- for benches \bench -> do
--         runReporters reporters \rep -> rep.onBenchStart bench.benchName
--         r <- for groupOpts.sizes \size -> do
--           runReporters reporters \rep -> rep.onSizeStart size

--           let benchOpts = mkDefaultBenchOpts groupOpts

--           { duration, iterations } /\ output <- bench.run
--             { size
--             , iterations: groupOpts.iterations
--             , reporters
--             }

--           runReporters reporters \rep -> rep.onSizeFinish { size, duration, iterations }

--           liftEffect $ itf.add size { benchName: bench.benchName, output }

--           pure ({ size, duration, iterations } /\ output)

--         pure { benchName: bench.benchName, samples: map fst r }

--       let groupResults = { groupName, benchs: results }

--       outs <- liftEffect itf.get
--       for_ outs $ checkResults groupOpts reporters

--       runReporters reporters \rep -> rep.onGroupFinish groupResults

--       pure groupResults
--   }

-- -- data Foo out = Foo { check :: GroupOpts ninp out -> ResultPerSize out -> Aff Unit, results :: ResultPerSize out }

benchImpl :: forall m a b. Eq b => MonadBench m => String -> (BenchOpts -> BenchOpts) -> (a -> m b) -> Bench a b
benchImpl benchName mkOpts benchFn = Bench $ notOnly
  { benchName
  , run: \{iterations } input -> do

      let opts@{ overrideIterations: iterations } = mkOpts defaultBenchOpts

      durs :: NonEmptyList _ <- replicate1A iterations do

        (_ /\ duration) <- measureTime \_ -> do
          toAff $ benchFn input

        pure duration

      let duration = calcMean durs

      let
        sampleResult =
          { iterations
          , duration
          }

      output <- toAff do
        benchFn input

      pure (sampleResult /\ output)
  }

-- type ResultPerSize out = { outputs :: Array { output :: out, benchName :: String }, size :: Size }

-- checkResults :: forall ninp out. Show out => Eq out => GroupOpts ninp out -> Array Reporter -> ResultPerSize out -> Aff Unit
-- checkResults groupOpts reporters all@{ size } =
--   for_ groupOpts.checkOutputs \check -> do

--     if check { size, outputs: map _.output all.outputs } then do
--       runReporters reporters \rep -> rep.onCheckResults
--         { success: true
--         , size
--         , outputs
--         }
--     else do
--       runReporters reporters \rep -> rep.onCheckResults
--         { success: false
--         , size
--         , outputs
--         }
--       throwError (error "Benchmarks results are not equal")
--   where
--   outputs = map (\val -> { strOutput: show val.output, benchName: val.benchName }) all.outputs

-- --- Typeclasses

-- -- | A combinator for entities that can be run only once.
-- -- | This is useful while debugging or developing benchmarks.
-- -- | So you do not have to run the whole suite again and again.
-- class CanRunOnly a where
--   only :: Warn (Text "`only` usage") => a -> a

-- instance CanRunOnly (Bench a) where
--   only (Bench mayOnly) = Bench (mayOnly { only = true })

-- instance CanRunOnly Group where
--   only (Group mayOnly) = Group (mayOnly { only = true })

-- | A class for monadic benchmarks.
-- | It allows to run benchmarks in different monads as long as they are capable of
-- | getting turned into an Aff monad.
class Monad m <= MonadBench m where
  toAff :: forall a. m a -> Aff a

instance MonadBench Effect where
  toAff = liftEffect

instance MonadBench Aff where
  toAff = identity

-- --- API shortcuts

-- -- | Create a benchmark of a given name.
-- -- | The benchmark will be run with the provided options.
-- -- | The benchmark is a function that takes an input and returns an output.
-- -- | The input is generated by the prepare function, and the output is processed by the finalize function.
-- bench :: forall arg inp ret out. Eq out => String -> (BenchOptsPure Size Size ret Unit -> BenchOptsPure arg inp ret out) -> (arg -> ret) -> Bench out
-- bench name mkOpts benchFn = benchImpl name (benchOptsPureToAff @Effect <<< mkOpts) (pure <<< benchFn)

-- -- | Like `bench`, but with default options.
-- bench_ :: forall ret. String -> (Size -> ret) -> Bench Unit
-- bench_ name benchFn = bench name identity (benchFn >>> const unit)

-- -- | Like `bench``, but with a monadic function.
-- benchM :: forall m arg inp ret out. MonadBench m => Eq out => String -> (BenchOptsM m Size Size ret Unit -> BenchOptsM m arg inp ret out) -> (arg -> m ret) -> Bench out
-- benchM name mkOpts benchFn = benchImpl name (mkOpts <<< benchOptsPureToAff) benchFn

-- -- | Like `benchM`, but with default options.
-- benchM_ :: forall @m out. Eq out => MonadBench m => String -> (Size -> m out) -> Bench Unit
-- benchM_ name benchFn = benchM name identity (benchFn >>> map (const unit))

-- -- | Like `benchSuite`, but with default options.
-- benchSuite_ :: String -> Array Group -> Suite
-- benchSuite_ groupName benchmarks = benchSuite groupName identity benchmarks

-- -- | Like `benchGroup`, but with default options.
-- benchGroup_ :: String -> Array (Bench Unit) -> Group
-- benchGroup_ groupName benches = benchGroup groupName identity benches

-- ---

-- --- Utils

-- runReporters :: Array Reporter -> (Reporter -> Effect Unit) -> Aff Unit
-- runReporters reporters f = liftEffect $ for_ reporters f

-- benchOptsPureToAff :: forall @m ninp inp bout out. Applicative m => BenchOptsPure ninp inp bout out -> BenchOptsM m ninp inp bout out
-- benchOptsPureToAff { overrideIterations, prepare, finalize } =
--   { overrideIterations
--   , prepare: \size -> pure $ prepare size
--   , normalizeOutput: \result -> pure $ finalize result
--   }

-- mayGetOnlies :: forall a. Array (MayOnly a) -> Array a
-- mayGetOnlies mayOnlies =
--   let
--     onlys = filter _.only mayOnlies
--   in
--     map _.val $ if Array.null onlys then mayOnlies else onlys

notOnly :: forall a. a -> MayOnly a
notOnly a = MayOnly { only: false, val: a }

measureTime :: forall a m. MonadEffect m => (Unit -> m a) -> m (a /\ Milliseconds)
measureTime action = do
  startTime <- liftEffect now
  result <- action unit
  endTime <- liftEffect now
  let duration = unwrap (unInstant endTime) - unwrap (unInstant startTime)
  pure (result /\ Milliseconds duration)

calcMean :: NonEmptyList Milliseconds -> Milliseconds
calcMean items = Milliseconds (sum (map coerce items :: NonEmptyList Number) / Int.toNumber (NEL.length items))

-- asciColorStr :: Int -> String -> String
-- asciColorStr color str =
--   let
--     colorCode = "\x1b[" <> show color <> "m"
--     resetCode = "\x1b[0m"
--   in
--     colorCode <> str <> resetCode

-- bgGray :: Int
-- bgGray = 100

-- memoizeEffect :: forall a b. Ord a => (a -> b) -> Effect (a -> Effect b)
-- memoizeEffect f = do
--   cacheRef <- Ref.new Map.empty
--   pure \x -> do
--     cache <- Ref.read cacheRef
--     case Map.lookup x cache of
--       Just result -> pure result
--       Nothing -> do
--         let result = f x
--         Ref.modify_ (Map.insert x result) cacheRef
--         pure result

-- -- Reporter

-- -- | Default reporter useful for selective overriding.
-- -- | It will do nothing.
-- defaultReporter :: Reporter
-- defaultReporter =
--   { onSuiteStart: const $ pure unit
--   , onGroupStart: const $ pure unit
--   , onSizeStart: const $ pure unit
--   , onBenchStart: const $ pure unit
--   , onSuiteFinish: const $ pure unit
--   , onGroupFinish: const $ pure unit
--   , onSizeFinish: const $ pure unit
--   , onBenchFinish: const $ pure unit
--   , onCheckResults: const $ pure unit
--   }

-- -- | Console reporter.
-- -- | It will print the results to the console in human readable format.
-- reportConsole :: Reporter
-- reportConsole = defaultReporter
--   { onSuiteStart = \name -> Console.log
--       ("• suite: " <> name)

--   , onGroupStart = \name -> Console.log
--       ("  • group: " <> name)

--   , onSizeFinish = \{ size, duration, iterations } -> Console.log
--       ( "      • " <> printStats
--           [ "size" /\ Int.toStringAs Int.decimal size
--           , "avg" /\ printMs duration
--           , "count" /\ Int.toStringAs Int.decimal iterations
--           ]
--       )

--   , onBenchStart = \benchName -> Console.log
--       ("    • " <> (asciColorStr bgGray ("bench: " <> benchName)))

--   -- , onBenchFinish = \{ duration: dur, iterations } -> Console.log
--   --     ("        • mean duration: " <> printMs dur <> " ms (" <> show iterations <> " iterations)\n")

--   , onCheckResults = \{ success, size, outputs } ->
--       if success then Console.log
--         ("    • check: ✓")
--       else do
--         Console.log
--           ("    • check: ✗")
--         for_ outputs \{ benchName, strOutput } -> Console.log
--           ("      • " <> benchName <> ": " <> show size)
--   }

-- printStats :: Array (String /\ String) -> String
-- printStats stats = Str.joinWith ", " (map (\(k /\ v) -> k <> "=" <> v) stats)

-- printMs :: Milliseconds -> String
-- printMs (Milliseconds ms) = NumFmt.toStringWith (NumFmt.fixed 4) ms <> "ms"

-- padLeft :: Int -> String -> String
-- padLeft n str =
--   let
--     len = Str.length str
--     pad = Str.joinWith "" $ replicate (n - len) " "
--   in
--     pad <> str

-- padRight :: Int -> String -> String
-- padRight n str =
--   let
--     len = Str.length str
--     pad = Str.joinWith "" $ replicate (n - len) " "
--   in
--     str <> pad