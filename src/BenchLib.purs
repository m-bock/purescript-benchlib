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
  , BenchOpts
  , Stats
  , Reporter
  -- , Size
  , benchSuite
  , benchGroup
  , bench
  , benchM
  , benchSuite_
  , benchGroup_
  , benchM_
  , bench_
  , checkEq
  , class MonadBench
  , toAff
  -- , class CanRunOnly
  -- , only
  , defaultReporter
  , reportConsole
  , run
  -- , eval
  ) where

import Prelude

import Data.Array (filter, foldr)
import Data.Array as Array
import Data.Bifunctor (class Bifunctor, lmap)
import Data.DateTime.Instant (unInstant)
import Data.Int as Int
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Number as Number
import Data.Number.Format as NumFmt
import Data.Semigroup.Foldable (maximum, minimum)
import Data.String as Str
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, sum)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicate)
import Data.Unfoldable1 (replicate1A)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Effect.Ref as Ref
import Prim.TypeError (class Warn, Text)
import Record as Record
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

--- Type Aliases

type Size = Int

--- Option Types

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
  , checkInputs :: Maybe ({ inputs :: Array a, size :: Size } -> Boolean)
  , checkOutputs :: Maybe ({ outputs :: Array b, size :: Size } -> Boolean)
  }

-- | Options for benchmarks.
type BenchOpts a =
  { iterations :: Int
  , prepareInput :: Size -> a
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

type SampleResult = { size :: Size, stats :: Stats }

--- Opaque types

-- | Opaque type for the benchmark suite.
newtype Suite = Suite
  { suiteName :: String
  , runSuite ::
      { iterations :: Int
      , reporter :: Reporter
      , sizes :: Array Size
      }
      -> Aff { groupResults :: Array GroupResult }
  }

-- | Opaque type for the benchmark group.
newtype Group = Group
  ( MayOnly
      { groupName :: String
      , runGroup ::
          { reporter :: Reporter
          , iterations :: Int
          , sizes :: Array Int
          }
          -> Aff
               { benchResults ∷ Array BenchResult
               , checkInputsResults :: Maybe (Array CheckResults)
               , checkOutputsResults :: Maybe (Array CheckResults)
               }
      }
  )

-- | Opaque type for the benchmark.
newtype Bench a b = Bench
  ( MayOnly
      { benchName :: String
      , runBench ::
          { iterations :: Int
          , size :: Size
          }
          -> Aff { stats :: Stats, input :: a, output :: b }
      }
  )

derive instance Functor (Bench a)

instance Bifunctor Bench where
  bimap f g (Bench mayOnly) = Bench $ mayOnly # map \rec -> rec
    { runBench = \opts -> do
        ret@{ input, output } <- rec.runBench opts
        pure $ ret { input = f input, output = g output }
    }

mapInput :: forall a a' b. (a -> a') -> Bench a b -> Bench a' b
mapInput = lmap

mapOutput :: forall a b b'. (b -> b') -> Bench a b -> Bench a b'
mapOutput = map

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
  , values :: Array { showedVal :: String, benchName :: String }
  }

--- Internal Types

type BenchName = String

type GroupName = String

newtype MayOnly a = MayOnly { only :: Boolean, val :: a }

derive instance Functor MayOnly

setOnly :: forall a. Boolean -> MayOnly a -> MayOnly a
setOnly b (MayOnly r) = MayOnly $ r { only = b }

--- Running the benchmark suite

-- | Run the benchmark suite.
run :: (RunOpts -> RunOpts) -> Suite -> Effect Unit
run mkOpts (Suite { runSuite, suiteName }) = launchAff_ do
  let
    runOpts = mkOpts defaultRunOpts
    reporter = foldr (<>) defaultReporter runOpts.reporters

  void $ runWrapped
    { before: reporter.onSuiteStart suiteName
    , after: reporter.onSuiteFinish
    }
    ( do
        { groupResults } <- runSuite
          { reporter
          , iterations: defaultSuiteOpts.iterations
          , sizes: defaultSuiteOpts.sizes
          }
        pure
          { groupResults
          , suiteName
          }
    )

runWrapped :: forall m a. Monad m => { before :: m Unit, after :: a -> m Unit } -> m a -> m a
runWrapped { before, after } action = do
  before
  result <- action
  after result
  pure result

run_ :: Suite -> Effect Unit
run_ suite = run identity suite

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

defaultRunOpts :: RunOpts
defaultRunOpts =
  { reporters: [ reportConsole ]
  }

defaultSuiteOpts :: SuiteOpts
defaultSuiteOpts =
  { sizes: [ 0, 25_000, 50_000, 100_000 ]
  , iterations: 1000
  }

mkDefaultGroupOpts :: forall a b. { sizes :: Array Size, iterations :: Int } -> GroupOpts a b
mkDefaultGroupOpts { sizes, iterations } =
  { sizes
  , iterations
  , checkInputs: Nothing
  , checkOutputs: Nothing
  }

mkDefaultBenchOpts :: { iterations :: Int } -> BenchOpts Size
mkDefaultBenchOpts { iterations } =
  { iterations
  , prepareInput: \size -> size
  }

--- Core functions

-- | Create a benchmark suite of a given name.
-- | The suite will be run with the provided options.
-- | The suite is a collection of groups, each containing multiple benchmarks.
benchSuite :: String -> (SuiteOpts -> SuiteOpts) -> Array Group -> Suite
benchSuite suiteName mkOpts groups_ = Suite
  { suiteName
  , runSuite: \{ reporter } -> do
      let { iterations, sizes } = mkOpts defaultSuiteOpts

      let groups = mayGetOnlies (map (\(Group g) -> g) groups_)

      do
        -- reporter.onSuiteStart suiteName

        groupResults <- for groups \{ runGroup, groupName } -> do
          reporter.onGroupStart groupName
          { benchResults, checkOutputsResults, checkInputsResults } <- runGroup { reporter, iterations, sizes }
          let
            groupResult =
              { groupName
              , benchResults
              , checkOutputsResults
              , checkInputsResults
              }
          reporter.onGroupFinish groupResult
          pure groupResult

        let suiteResult = { groupResults }

        --reporter.onSuiteFinish suiteResult

        pure suiteResult
  }

type PerSizeItf a b =
  { add :: { size :: Size, benchName :: String, input :: a, output :: b } -> Aff Unit
  , getCheckOutputsResults :: Aff (Maybe (Array CheckResults))
  , getCheckInputsResults :: Aff (Maybe (Array CheckResults))
  }

mkPerSizeItf :: forall a b. Show a => Show b => GroupOpts a b -> Aff (PerSizeItf a b)
mkPerSizeItf groupOpts = liftEffect do
  refOutputs <- Ref.new (Map.empty :: Map Size (ResultPerSize a b))

  pure
    { add: \{ size, benchName, input, output } -> liftEffect $ Ref.modify_
        ( Map.insertWith (<>) size
            { inputs: [ { benchName, input } ]
            , outputs: [ { benchName, output } ]
            }
        )
        refOutputs

    , getCheckOutputsResults: liftEffect do
        ret <- Ref.read refOutputs
        let perSize = map (\(size /\ val) -> Record.merge { size } val) $ Map.toUnfoldable ret

        for groupOpts.checkOutputs \checkOutputs -> pure
          ( map
              ( \{ size, outputs } ->
                  { success: checkOutputs { outputs: map _.output outputs, size }
                  , size
                  , values: map (\{ benchName, output } -> { benchName, showedVal: show output }) outputs
                  }
              )
              perSize
          )

    , getCheckInputsResults: liftEffect do
        ret <- Ref.read refOutputs
        let perSize = map (\(size /\ val) -> Record.merge { size } val) $ Map.toUnfoldable ret

        for groupOpts.checkInputs \checkInputs -> pure
          ( map
              ( \{ size, inputs } ->
                  { success: checkInputs { inputs: map _.input inputs, size }
                  , size
                  , values: map (\{ benchName, input } -> { benchName, showedVal: show input }) inputs
                  }
              )
              perSize
          )
    }

-- | Create a benchmark group of a given name.
-- | The group will be run with the provided options.
-- | The group is a collection of benchmarks
benchGroup :: forall a b. Show a => Show b => String -> (GroupOpts a b -> GroupOpts a b) -> Array (Bench a b) -> Group
benchGroup groupName mkOpts benches_ =
  Group $ notOnly
    { groupName
    , runGroup: \defOpts@{ reporter } -> do
        --reporter.onGroupStart groupName

        let
          groupOpts@{ sizes, iterations } = mkOpts $ mkDefaultGroupOpts
            { sizes: defOpts.sizes
            , iterations: defOpts.iterations
            }

        let
          benches = mayGetOnlies $ map (\(Bench b) -> b) benches_

        perSizeItf <- mkPerSizeItf groupOpts

        benchResults <- for benches
          ( \{ benchName, runBench } -> do
              --reporter.onBenchStart benchName
              samples <- for sizes
                ( \size -> do

                    { stats, output, input } <- runWrapped
                      { before: reporter.onSampleStart size
                      , after: \{stats} -> reporter.onSampleFinish {size, stats }
                      }
                      (runBench { iterations, size })

                    perSizeItf.add { size, benchName, output, input }

                    pure { size, stats }
                )

              pure { benchName, samples }
          )

        checkOutputsResults <- perSizeItf.getCheckOutputsResults
        checkInputsResults <- perSizeItf.getCheckOutputsResults

        pure { benchResults, checkOutputsResults, checkInputsResults }
    }

benchImpl :: forall m a b. Eq b => MonadBench m => String -> (BenchOpts Size -> BenchOpts a) -> (a -> m b) -> Bench a b
benchImpl benchName mkOpts benchFn =
  Bench $ notOnly
    { benchName
    , runBench: \defOpts@{ size } -> do

        let { iterations, prepareInput } = mkOpts $ mkDefaultBenchOpts { iterations: defOpts.iterations }

        durs :: NonEmptyList _ <- replicate1A iterations
          ( do
              let input = prepareInput size

              duration <- measureTime \_ -> do
                toAff $ benchFn input

              pure duration
          )

        let input = prepareInput size

        output <- toAff $ benchFn input

        let stats = calcStats durs

        pure { stats, output, input }
    }

type ResultPerSize a b =
  { inputs :: Array { input :: a, benchName :: String }
  , outputs :: Array { output :: b, benchName :: String }
  }

-- checkResults
--   :: forall a b
--    . Show a
--   => Show b
--   => { groupOpts :: GroupOpts a b
--      , reporter :: Reporter
--      , size :: Size
--      , inputs :: Array { input :: a, benchName :: String }
--      , outputs :: Array { output :: b, benchName :: String }
--      }
--   -> Aff Unit
-- checkResults { groupOpts, reporter, size, inputs, outputs } =
--   for_ groupOpts.checkOutputs \check -> do

--     if check { size, outputs: map _.output outputs } then do
--       reporter.onCheckResults
--         { success: true
--         , size
--         , outputs: outputs'
--         }
--     else do
--       reporter.onCheckResults
--         { success: false
--         , size
--         , outputs: outputs'
--         }
--       throwError (error "Benchmarks results are not equal")
--   where
--   outputs' = map (\val -> { strOutput: show val.output, benchName: val.benchName }) outputs

--- Typeclasses

-- | A combinator for entities that can be run only once.
-- | This is useful while debugging or developing benchmarks.
-- | So you do not have to run the whole suite again and again.
class CanRunOnly a where
  only :: Warn (Text "`only` usage") => a -> a

instance CanRunOnly (Bench a b) where
  only (Bench mayOnly) = Bench $ setOnly true mayOnly

instance CanRunOnly Group where
  only (Group mayOnly) = Group $ setOnly true mayOnly

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
bench :: forall a b. Eq b => String -> (BenchOpts Size -> BenchOpts a) -> (a -> b) -> Bench a b
bench name mkOpts benchFn = benchImpl name mkOpts (pure @Effect <<< benchFn)

-- | Like `bench`, but with default options.
bench_ :: forall b. Eq b => String -> (Size -> b) -> Bench Size b
bench_ name benchFn = bench name identity benchFn

-- | Like `bench``, but with a monadic function.
benchM :: forall m a b. MonadBench m => Eq b => String -> (BenchOpts Size -> BenchOpts a) -> (a -> m b) -> Bench a b
benchM name mkOpts benchFn = benchImpl name mkOpts benchFn

-- | Like `benchM`, but with default options.
benchM_ :: forall m b. MonadBench m => Eq b => String -> (Size -> m b) -> Bench Size b
benchM_ name benchFn = benchM name identity benchFn

-- | Like `benchSuite`, but with default options.
benchSuite_ :: String -> Array Group -> Suite
benchSuite_ groupName benchmarks = benchSuite groupName identity benchmarks

-- | Like `benchGroup`, but with default options.
benchGroup_ :: forall b. Show b => Eq b => String -> Array (Bench Size b) -> Group
benchGroup_ groupName benches = benchGroup groupName identity benches

---

--- Utils

mayGetOnlies :: forall a. Array (MayOnly a) -> Array a
mayGetOnlies mayOnlies_ =
  let
    mayOnlies = map (\(MayOnly mo) -> mo) mayOnlies_
    onlys = filter _.only mayOnlies
  in
    map _.val $ if Array.null onlys then mayOnlies else onlys

notOnly :: forall a. a -> MayOnly a
notOnly a = MayOnly { only: false, val: a }

measureTime :: forall a m. MonadEffect m => (Unit -> m a) -> m Milliseconds
measureTime action = do
  startTime <- liftEffect now
  _ <- action unit
  endTime <- liftEffect now
  let duration = unwrap (unInstant endTime) - unwrap (unInstant startTime)
  pure (Milliseconds duration)

calcMean :: NonEmptyList Milliseconds -> Milliseconds
calcMean items = Milliseconds (sum (map coerce items :: NonEmptyList Number) / Int.toNumber (NEL.length items))

calcMedian :: NonEmptyList Milliseconds -> Milliseconds
calcMedian items = unsafeCoerce 1

-- let
--   sorted = List.sort (NEL.toList items)
--   n = NEL.length items
-- in
--   if n `mod` 2 == 0 then
--     Milliseconds ((sorted !! (n `div` 2 - 1) + sorted !! (n `div` 2)) / 2.0)
--   else
--     Milliseconds (sorted !! (n `div` 2))

calcStdDev :: NonEmptyList Milliseconds -> Milliseconds -> Milliseconds
calcStdDev items (Milliseconds mean) =
  let
    n = Int.toNumber (NEL.length items)
    variance = sum (map (\(Milliseconds x) -> (x - mean) `Number.pow` 2.0) items) / n
  in
    Milliseconds (Number.sqrt variance)

type Stats =
  { mean :: Milliseconds
  , min :: Milliseconds
  , max :: Milliseconds
  , stddev :: Milliseconds
  }

calcStats :: NonEmptyList Milliseconds -> Stats
calcStats items =
  let
    mean = calcMean items
    min = minimum items
    max = maximum items
    stddev = calcStdDev items mean
  in
    { mean, min, max, stddev }

asciColorStr :: Int -> String -> String
asciColorStr color str =
  let
    colorCode = "\x1b[" <> show color <> "m"
    resetCode = "\x1b[0m"
  in
    colorCode <> str <> resetCode

bgGray :: Int
bgGray = 100

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

  --, onSizeFinish = unsafeCoerce 1
  --  \{ size, duration, iterations } -> Console.log
  --     ( "      • " <> printStats
  --         [ "size" /\ Int.toStringAs Int.decimal size
  --         , "avg" /\ printMs duration
  --         , "count" /\ Int.toStringAs Int.decimal iterations
  --         ]
  --     )

  , onBenchStart = \benchName -> Console.log
      ("    • " <> (asciColorStr bgGray ("bench: " <> benchName)))

  -- , onBenchFinish = \{ duration: dur, iterations } -> Console.log
  --     ("        • mean duration: " <> printMs dur <> " ms (" <> show iterations <> " iterations)\n")

  -- , onCheckResults = \{ success, size, outputs } ->
  --     if success then Console.log
  --       ("    • check: ✓")
  --     else do
  --       Console.log
  --         ("    • check: ✗")
  --       for_ outputs \{ benchName, strOutput } -> Console.log
  --         ("      • " <> benchName <> ": " <> show size)
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