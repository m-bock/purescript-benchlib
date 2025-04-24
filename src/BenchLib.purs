module BenchLib
  ( Bench
  , BenchOpts
  , BenchBaseOpts
  , BenchBaseOptsAff
  , BenchOptsAff
  , BenchResult
  , CheckResult(..)
  , SizeCheckResult
  , BenchCheckResult
  , Group
  , GroupOpts
  , GroupResult
  , Reporter
  , RunOpts
  , SampleResult
  , Size
  , Suite
  , SuiteOpts
  , SuiteResult
  , bench
  , benchAff
  , benchAff_
  , bench_
  , class CanRunOnly
  , defaultReporter
  , group
  , group_
  , only
  , reportConsole
  , runNode
  , runNode_
  , suite
  , suite_
  ) where

import Prelude

import Data.Array (filter, foldr)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), isLeft)
import Data.Filterable (filterMap)
import Data.Foldable (all, findMap, for_)
import Data.FoldableWithIndex (findMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Number.Format as NumFmt
import Data.String as Str
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (range)
import Data.Unfoldable1 (replicate1A)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Effect.Ref as Ref
import Node.Process as Process
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
  , check :: Maybe (Size -> Array (a /\ b) -> Boolean)
  , printInput :: Maybe (a -> String)
  , printOutput :: Maybe (b -> String)
  }

-- | Options for pure benchmarks.
type BenchOpts a' b' a b =
  { iterations :: Int
  , normIn :: a -> a'
  , normOut :: b -> b'
  }

-- | Options for effectful benchmarks.
type BenchOptsAff a' b' a b =
  { iterations :: Int
  , normIn :: a -> Aff a'
  , normOut :: b -> Aff b'
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
  , checkResult :: CheckResult
  }

data CheckResult
  = NotChecked
  | CheckedSuccess
  | CheckedFailure { firstFailure :: SizeCheckResult }

derive instance Eq CheckResult

derive instance Generic CheckResult _

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

type SizeCheckResult =
  { groupName :: String
  , size :: Size
  , benchResult :: Array BenchCheckResult
  }

type BenchCheckResult =
  { showedInput :: Maybe String
  , showedOutput :: Maybe String
  , benchName :: String
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
      -> Aff (Maybe SuiteResult)
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

--- Internal Types

type BenchName = String

type GroupName = String

--- Running the benchmark suite

-- | Run the benchmark suite.
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
    Nothing -> pure unit
    Just _ -> liftEffect $ Process.exit' 1

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
  , check: Nothing
  , printInput: Nothing
  , printOutput: Nothing
  }

mkDefaultBenchOpts :: forall a b. { iterations :: Size } -> BenchOpts Unit Unit a b
mkDefaultBenchOpts { iterations } =
  { iterations
  , normIn: const unit
  , normOut: const unit
  }

mkDefaultBenchOptsAff :: forall a b. { iterations :: Size } -> BenchOptsAff Unit Unit a b
mkDefaultBenchOptsAff { iterations } =
  { iterations
  , normIn: const (pure unit)
  , normOut: const (pure unit)
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
        runGroup { reporter, iterations, sizes }

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
    failures = filter
      ( _.checkResult >>> case _ of
          CheckedFailure _ -> true
          _ -> false
      )
      groupResults
  in
    Array.length failures == 0

type PerSizeItf a b =
  { addEntry :: { size :: Size, benchName :: String, input :: a, output :: b } -> Aff Unit
  , getCheckResults :: Aff CheckResult
  }

mkPerSizeItf :: forall a b. GroupName -> GroupOpts a b -> Aff (PerSizeItf a b)
mkPerSizeItf groupName groupOpts@{ printInput, printOutput } = liftEffect do
  refAccum <- Ref.new (Map.empty :: Map Size (ResultPerSize a b))

  pure
    { addEntry: \{ size, benchName, input, output } -> liftEffect $ Ref.modify_
        ( Map.insertWith (<>) size [ { benchName, input, output } ]
        )
        refAccum

    , getCheckResults: do
        accum <- liftEffect $ Ref.read refAccum

        pure $ mkCheckResult accum
    }

  where
  mkCheckResult :: Map Size (ResultPerSize a b) -> CheckResult
  mkCheckResult mp = case groupOpts.check of
    Nothing -> NotChecked
    Just check ->
      let
        mayResult :: Maybe { firstFailure :: SizeCheckResult }
        mayResult = mp
          # findMapWithIndex
              ( \size r ->
                  let
                    isSuccess = check size (map (\{ input, output } -> input /\ output) r)
                  in
                    if isSuccess then Nothing
                    else Just
                      { firstFailure: mkSizeCheckResult size r
                      }
              )
      in
        case mayResult of
          Nothing -> CheckedSuccess
          Just x -> CheckedFailure x

    where
    mkSizeCheckResult :: Size -> Array { benchName ∷ String, input ∷ a, output ∷ b } -> SizeCheckResult
    mkSizeCheckResult size r =
      { size
      , groupName
      , benchResult: map
          ( \{ benchName, input, output } ->
              { benchName
              , showedInput: map (\f -> f input) groupOpts.printInput
              , showedOutput: map (\f -> f output) groupOpts.printOutput
              }
          )
          r
      }

-- | Create a benchmark group of a given name.
-- | The group will be run with the provided options.
-- | The group is a collection of benchmarks
group
  :: forall a b
   . String
  -> (GroupOpts a b -> GroupOpts a b)
  -> Array (Bench a b)
  -> Group
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

                    perSizeItf.addEntry { size, benchName, output, input }

                    reporter.onSampleFinish sampleResult
                    pure sampleResult
                )

              let benchResult = { benchName, samples }

              reporter.onBenchFinish benchResult
              pure benchResult
          )

        checkResult <- perSizeItf.getCheckResults

        let groupResult = { groupName, benchResults, checkResult }

        reporter.onGroupFinish groupResult

        pure groupResult
    }

bench
  :: forall a' b' a b
   . String
  -> (BenchOpts Unit Unit a b -> BenchOpts a' b' a b)
  -> BenchBaseOpts a b
  -> Bench a' b'
bench benchName mkOpts { prepare, run } =
  Bench
    { benchName
    , only: false
    , runBench: \{ size, iterations } -> do
        let { iterations, normIn, normOut } = mkOpts $ mkDefaultBenchOpts { iterations }

        sampleResult <- do
          let
            input = prepare size

          duration <- measureTime \_ -> do
            for_ (range 0 iterations :: Array _) \_ -> do
              let _output = run input
              pure unit

          let average = Milliseconds (unwrap duration / Int.toNumber iterations)

          pure { size, average, iterations }

        { inputNorm, outputNorm } <- do
          let
            input = prepare size :: a
            inputNorm = normIn input :: a'

            output = run input :: b
            outputNorm = normOut output :: b'

          pure { inputNorm, outputNorm }

        pure
          { sampleResult
          , input: inputNorm
          , output: outputNorm
          }

    }

benchAff :: forall a' b' a b. String -> (BenchOptsAff Unit Unit a b -> BenchOptsAff a' b' a b) -> BenchBaseOptsAff a b -> Bench a' b'
benchAff benchName mkOpts { prepare, run } =
  Bench
    { benchName
    , only: false
    , runBench: \{ size, iterations } -> do
        let { iterations, normIn, normOut } = mkOpts $ mkDefaultBenchOptsAff { iterations }

        sampleResult <- do

          inputs :: NonEmptyArray a <- replicate1A iterations (prepare size)

          duration <- measureTime \_ -> do
            _results :: NonEmptyArray b <- for inputs run
            pure unit

          let average = Milliseconds (unwrap duration / Int.toNumber iterations)

          pure { size, average, iterations }

        { outputNorm, inputNorm } <- do
          input <- prepare size
          inputNorm <- normIn input

          output <- run input
          outputNorm <- normOut output

          pure { outputNorm, inputNorm }

        pure
          { sampleResult
          , input: inputNorm
          , output: outputNorm
          }

    }

type ResultPerSize a b = Array
  { input :: a
  , output :: b
  , benchName :: String
  }

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

--- API shortcuts

type BenchBaseOpts a b =
  { prepare :: Size -> a
  , run :: a -> b
  }

type BenchBaseOptsAff a b =
  { prepare :: Size -> Aff a
  , run :: a -> Aff b
  }

bench_ :: forall a b. String -> BenchBaseOpts a b -> Bench Unit Unit
bench_ name baseOpts = bench name identity baseOpts

benchAff_ :: forall a b. String -> BenchBaseOptsAff a b -> Bench Unit Unit
benchAff_ name baseOpts = benchAff name identity baseOpts

runNode_ :: Suite -> Effect Unit
runNode_ = runNode identity

-- | Like `suite`, but with default options.
suite_ :: String -> Array Group -> Suite
suite_ groupName benchmarks = suite groupName identity benchmarks

-- | Like `group`, but with default options.
group_ :: forall a b. String -> Array (Bench a b) -> Group
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

bold :: Int
bold = 1

italics :: Int
italics = 3

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
      (asciColorStr bold ("• Suite: " <> name))

  , onGroupStart = \name -> Console.log
      (asciColorStr bold ("  • Group: " <> name))

  , onBenchStart = \benchName -> Console.log
      (asciColorStr bold "    • " <> (asciColorStr bold $ asciColorStr bgGray ("Bench: " <> benchName)))

  , onBenchFinish = \_ -> Console.log ""

  , onGroupFinish = \{ checkResult } -> do
      let
        msg = case checkResult of
          NotChecked -> "⚠ not checked"
          CheckedSuccess -> "✔ check success"
          CheckedFailure { firstFailure: { size } } -> "✖ check failure for sizes " <> show size

      Console.log ("    " <> asciColorStr bold msg <> "\n")

      Console.log ("")

  , onSampleFinish = \{ size, average, iterations } -> Console.log
      ( asciColorStr bold "      » " <> printStats " "
          [ "size" /\ Int.toStringAs Int.decimal size /\ Nothing
          , "count" /\ Int.toStringAs Int.decimal iterations /\ Nothing
          , "avg" /\ printMs average /\ Just "ms"
          ]
      )

  , onSuiteFinish = \suiteResult -> do
      let summary = getSuiteSummary suiteResult
      let
        firstGroupFailure = suiteResult.groupResults
          # Array.findMap
              ( \{ checkResult } -> case checkResult of
                  CheckedFailure val -> Just val.firstFailure
                  _ -> Nothing
              )

      for_ firstGroupFailure \{ size, groupName, benchResult } -> do
        Console.log
          $ asciColorStr bold ("Check failure for size " <> show size <> " in group \"" <> groupName <> "\"")
        Console.log ""

        for_ benchResult \{ benchName, showedInput, showedOutput } -> do
          Console.log $ asciColorStr bold (benchName <> ":")
          Console.log ("input:")
          Console.log case showedInput of
            Just input -> input
            Nothing -> asciColorStr italics "Not printable. Provide `printInput` option."
          Console.log ("")

          Console.log ("output:")
          Console.log case showedOutput of
            Just output -> output
            Nothing -> asciColorStr italics "Not printable. Provide `printOutput` option."
          Console.log ""

      Console.log ""

      Console.log $ asciColorStr bold "Suite finished"
      Console.log
        ( printStats "\n"
            [ "groups" /\ Int.toStringAs Int.decimal summary.countGroups /\ Nothing
            , "checked" /\ Int.toStringAs Int.decimal summary.countCheckedGroups /\ Nothing
            , "failed" /\ Int.toStringAs Int.decimal summary.countFailedGroups /\ Nothing
            ]
        )
  }

getSuiteSummary :: SuiteResult -> SuiteSummary
getSuiteSummary { groupResults } =
  let
    checked = groupResults
      # filter (\{ checkResult } -> checkResult /= NotChecked)

    failures = filter
      ( _.checkResult >>> case _ of
          CheckedFailure _ -> true
          _ -> false
      )
      groupResults
  in
    { countGroups: Array.length groupResults
    , countCheckedGroups: Array.length checked
    , countFailedGroups: Array.length failures
    }

type SuiteSummary =
  { countGroups :: Int
  , countCheckedGroups :: Int
  , countFailedGroups :: Int
  }

-- type GroupSummary =
--   { checked :: Maybe (Maybe CheckResults)
--   }

printStats :: String -> Array (String /\ String /\ Maybe String) -> String
printStats sep stats = Str.joinWith sep (map (\(k /\ v /\ un) -> k <> "=" <> asciColorStr bold v <> fromMaybe "" un) stats)

printMs :: Milliseconds -> String
printMs (Milliseconds ms) = NumFmt.toStringWith (NumFmt.fixed 4) ms