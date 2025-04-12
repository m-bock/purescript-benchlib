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
  , BenchOptsAff
  , Reporter
  , suite
  , group
  , bench
  , benchAff
  , runNode_
  , suite_
  , group_
  , benchAff_
  , bench_
  , basic
  , normalizeAff
  , normalize
  , normalizeInputAff
  , normalizeOutputAff
  , normalizeInput
  , normalizeOutput
  , checkAllEq
  , class CanRunOnly
  , only
  , defaultReporter
  , reportConsole
  , runNode
  ) where

import Prelude

import Data.Array (filter, foldr)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.DateTime.Instant (unInstant)
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
import Node.Process as Process
import Prim.TypeError (class Warn, Text)

--- Type Aliases

type Size = Int

--- Option Types

-- | Options to run benchaffark suites.
type RunOpts =
  { reporters :: Array Reporter
  }

-- | Options for the benchaffark suite.
type SuiteOpts =
  { sizes :: Array Size
  , iterations :: Int
  }

-- | Options for the benchaffark group.
type GroupOpts a b =
  { sizes :: Array Int
  , iterations :: Int
  , checkInputs :: Maybe ({ results :: Array a, size :: Size } -> Boolean)
  , checkOutputs :: Maybe ({ results :: Array b, size :: Size } -> Boolean)
  }

-- | Options for pure benchaffarks.
type BenchOpts a =
  { iterations :: Int
  , prepareInput :: Size -> a
  }

-- | Options for monadic benchaffarks.
type BenchOptsAff (m :: Type -> Type) a =
  { iterations :: Int
  , prepareInput :: Size -> m a
  }

--- Result Types

-- | The result of a benchaffark suite.
type SuiteResult =
  { groupResults :: Array GroupResult
  , suiteName :: String
  }

-- | The result of a benchaffark group.
type GroupResult =
  { groupName :: String
  , benchResults :: Array BenchResult
  , checkOutputsResults :: Maybe (Array CheckResults)
  , checkInputsResults :: Maybe (Array CheckResults)
  }

-- | The result of a benchaffark.
type BenchResult =
  { benchName :: String
  , samples :: Array SampleResult
  }

-- | The result of one benchaffark sample.
type SampleResult =
  { iterations :: Int
  , size :: Size
  , average :: Milliseconds
  }

--- Opaque types

-- | Opaque type for the benchaffark suite.
newtype Suite = Suite
  { suiteName :: String
  , runSuite ::
      { iterations :: Int
      , reporter :: Reporter
      , sizes :: Array Size
      }
      -> Aff (Maybe SuiteResult)
  }

-- | Opaque type for the benchaffark group.
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

-- | Opaque type for the benchaffark.
newtype Bench a b = Bench
  { only :: Boolean
  , benchName :: String
  , runBench ::
      { iterations :: Int
      , size :: Size
      }
      -> Aff { sampleResult :: SampleResult, input :: Aff a, output :: Aff b }
  }

derive instance Functor (Bench a)

--- Reporter types

-- | A reporter is a set of functions that are called at different stages of the benchaffark.
-- | It allows to customize the output of the benchaffark suite or perform other actions like writing to a file.
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
  { groupName :: String
  , success :: Boolean
  , size :: Size
  , results :: Array { showedVal :: String, benchName :: String }
  }

--- Internal Types

type BenchName = String

type GroupName = String

--- Running the benchaffark suite

-- | Run the benchaffark suite.
runNode :: (RunOpts -> RunOpts) -> Suite -> Effect Unit
runNode mkRunOpts (Suite { runSuite }) = launchAff_ do
  let
    { reporters } = mkRunOpts defaultRunOpts

  -- Merge reporters into a single reporter:
  let
    reporter = foldr (<>) defaultReporter reporters

  let { iterations, sizes } = defaultSuiteOpts

  result <- runSuite { reporter, iterations, sizes }

  case result of
    Nothing -> liftEffect $ Process.exit' 1
    Just _ -> pure unit

--- Exported utility functions

-- | Check if all elements in the array are equal.
-- | Useful for checking the results of benchaffarks.
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

mkDefaultBenchOpts :: forall r. { iterations :: Int | r } -> BenchOpts Size
mkDefaultBenchOpts { iterations } =
  { iterations
  , prepareInput: \size -> size -- TODO: mk effectful
  }

benchOptsToM :: forall a. BenchOpts a -> BenchOptsAff Aff a
benchOptsToM { iterations, prepareInput } =
  { iterations
  , prepareInput: prepareInput >>> pure
  }

--- Core functions

-- | Create a benchaffark suite of a given name.
-- | The suite will be run with the provided options.
-- | The suite is a collection of groups, each containing multiple benchaffarks.
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

      pure
        if isSuccess groupResults then
          Nothing
        else
          Just suiteResult
  }

isSuccess :: Array GroupResult -> Boolean
isSuccess groupResults =
  let
    checkResults = groupResults
      # map
          ( \{ checkOutputsResults } -> case checkOutputsResults of
              Just xs -> xs
              Nothing -> []
          )
      # join

    failures = filter (\{ success } -> not success) checkResults
    nFailures = Array.length failures
  in
    nFailures == 0

type PerSizeItf a b =
  { addEntry :: { size :: Size, benchName :: String, input :: a, output :: b } -> Aff Unit
  , getCheckOutputsResults :: Aff (Maybe (Array CheckResults))
  , getCheckInputsResults :: Aff (Maybe (Array CheckResults))
  }

mkPerSizeItf :: forall a b. Show a => Show b => GroupName -> GroupOpts a b -> Aff (PerSizeItf a b)
mkPerSizeItf groupName groupOpts = liftEffect do
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
                , groupName
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
                { groupName
                , size
                , success: checkInputs { results: map _.value inputs, size }
                , results: map (\{ benchName, value } -> { benchName, showedVal: show value }) inputs
                }
            )
            (Map.toUnfoldable accum)
    }

-- | Create a benchaffark group of a given name.
-- | The group will be run with the provided options.
-- | The group is a collection of benchaffarks
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

        perSizeItf <- mkPerSizeItf groupName groupOpts

        benchResults <- for benches
          ( \(Bench { benchName, runBench }) -> do
              reporter.onBenchStart benchName
              samples <- for sizes
                ( \size -> do
                    reporter.onSampleStart size

                    { sampleResult, output, input } <- runBench { iterations, size }

                    output <- output
                    input <- input

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

benchImpl :: forall a b. String -> (BenchOpts Size -> BenchOptsAff Aff a) -> (a -> Aff b) -> Bench a b
benchImpl benchName mkBenchOpts benchFn =
  Bench
    { benchName
    , only: false
    , runBench: \defOpts@{ size } -> do

        let { iterations, prepareInput } = mkBenchOpts $ mkDefaultBenchOpts defOpts

        inputs :: NonEmptyArray _ <- replicate1A iterations (prepareInput size)

        duration <- measureTime \_ -> for inputs \input -> benchFn input

        let average = Milliseconds (unwrap duration / Int.toNumber iterations)

        input <- prepareInput size

        output <- benchFn input

        let sampleResult = { size, average, iterations }

        pure { sampleResult, output: pure output, input: pure input }
    }

type ResultPerSize a b =
  { inputs :: Array { value :: a, benchName :: String }
  , outputs :: Array { value :: b, benchName :: String }
  }

-- Normalization

normalizeAff :: forall a a' b b'. (a -> Aff a') -> (b -> Aff b') -> Bench a b -> Bench a' b'
normalizeAff normIn normOut (Bench rec) = Bench $ rec
  { runBench = \opts -> do
      ret@{ input, output } <- rec.runBench opts
      pure $ ret { input = input >>= normIn, output = output >>= normOut }
  }

normalize :: forall a a' b b'. (a -> a') -> (b -> b') -> Bench a b -> Bench a' b'
normalize normIn normOut = normalizeAff (pure <<< normIn) (pure <<< normOut)

normalizeInputAff :: forall a a' b. (a -> Aff a') -> Bench a b -> Bench a' b
normalizeInputAff norm = normalizeAff norm pure

normalizeOutputAff :: forall a b b'. (b -> Aff b') -> Bench a b -> Bench a b'
normalizeOutputAff norm = normalizeAff pure norm

normalizeInput :: forall a a' b. (a -> a') -> Bench a b -> Bench a' b
normalizeInput norm = normalize norm identity

normalizeOutput :: forall a b b'. (b -> b') -> Bench a b -> Bench a b'
normalizeOutput norm = normalize identity norm

basic :: forall a b. Bench a b -> Bench Unit Unit
basic = normalizeAff (const $ pure unit) (const $ pure unit)

--- Typeclasses

-- | A combinator for entities that can be run only once.
-- | This is useful while debugging or developing benchaffarks.
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

--- API shortcuts

bench :: forall a b. String -> (BenchOpts Size -> BenchOpts a) -> (a -> b) -> Bench a b
bench name mkOpts benchFn = benchImpl name mkOpts' (pure <<< benchFn)
  where
  mkOpts' :: BenchOpts Size -> BenchOptsAff Aff a
  mkOpts' optsPure = benchOptsToM $ mkOpts optsPure

bench_ :: forall b. String -> (Size -> b) -> Bench Size b
bench_ name benchFn = bench name identity benchFn

benchAff :: forall a b. String -> (BenchOptsAff Aff Size -> BenchOptsAff Aff a) -> (a -> Aff b) -> Bench a b
benchAff name mkOpts benchFn = benchImpl name mkOpts' benchFn
  where
  mkOpts' :: BenchOpts Size -> BenchOptsAff Aff a
  mkOpts' optsPure = mkOpts $ benchOptsToM optsPure

-- | Like `benchAff`, but with default options.
benchAff_ :: forall b. String -> (Size -> Aff b) -> Bench Size b
benchAff_ name benchFn = benchAff name identity benchFn

runNode_ :: Suite -> Effect Unit
runNode_ = runNode identity

-- | Like `suite`, but with default options.
suite_ :: String -> Array Group -> Suite
suite_ groupName benchaffarks = suite groupName identity benchaffarks

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

  , onGroupFinish = \{ groupName, benchResults, checkOutputsResults, checkInputsResults } -> do
      Console.log ("    • check:")
  -- for_ benchResults \{ benchName, samples } -> do
  --   Console.log ("    • " <> (asciColorStr bgGray ("bench: " <> benchName)))
  --   for_ samples \{ size, average, iterations } -> do
  --     Console.log
  --       ( "      • " <> printStats
  --           [ "size" /\ Int.toStringAs Int.decimal size
  --           , "count" /\ Int.toStringAs Int.decimal iterations
  --           , "avg" /\ printMs average
  --           ]
  --       )

  , onSuiteFinish = \_ -> Console.log "Suite finished"
  }

printStats :: Array (String /\ String) -> String
printStats stats = Str.joinWith ", " (map (\(k /\ v) -> k <> "=" <> v) stats)

printMs :: Milliseconds -> String
printMs (Milliseconds ms) = NumFmt.toStringWith (NumFmt.fixed 4) ms <> "ms"