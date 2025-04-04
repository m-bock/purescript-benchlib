module BenchLib.Reporters.Markdown
  ( Opts
  , reportMarkdown
  , reportMarkdown_
  ) where

import Prelude

import BenchLib (BenchResult, GroupResults, Reporter, SuiteResults, Size, defaultReporter)
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format as Number
import Data.Semigroup.Foldable (foldr1)
import Data.String as Str
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (unfoldr)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Node.Path (FilePath)

type Opts =
  { filePath :: FilePath
  , showHeadline :: Boolean
  }

defaultOpts :: Opts
defaultOpts = { 
  filePath: "bench-results.md" ,
  showHeadline: false
  }

reportMarkdown_ :: Reporter
reportMarkdown_ = reportMarkdown identity

reportMarkdown :: (Opts -> Opts) -> Reporter
reportMarkdown mkOpts =
  let
    opts = mkOpts defaultOpts
  in
    defaultReporter
      { onSuiteFinish = \suiteResults -> do
          writeTextFile UTF8 opts.filePath $ toSuiteMd opts suiteResults
          Console.error ("Wrote Markdown report to " <> opts.filePath)
      }

toSuiteMd :: Opts ->  SuiteResults -> String
toSuiteMd opts { groups, suiteName } = Str.joinWith "\n"
  [ if opts.showHeadline then "# " <> suiteName else ""
  , Str.joinWith "\n" $ map groupToMd groups
  ]

colors :: Array String
colors = [ "ff3456", "00ff00", "0000ff", "ffff00", "ff00ff", "00ffff" ]

groupToMd :: GroupResults -> String
groupToMd { benchs, groupName } = Str.joinWith "\n"
  [ "```mermaid"
  , "---"
  , "  config:"
  , "    themeVariables:"
  , "        xyChart:"
  , "            plotColorPalette: \"" <> Str.joinWith ", " (map ("#" <> _) colors)  <> "\""
  , "---"
  , "xychart-beta"
  , "  title \"" <> groupName <> "\""
  , "  x-axis \"Input Size\" [" <> getXTicks' sizes benchs <> "]"
  , "  y-axis \"Time (in ms)\" 0 --> 1"
  -- , "  line [100, 200, 300]"
  , Str.joinWith "\n" $ map (("  " <> _) <<< getLine' sizes) benchs
  , "```"
  , Str.joinWith "&nbsp;&nbsp;" $ mapWithIndex getLegendItem benchNames

  ]
  where 
    sizes = join $ map (_.samples >>> map _.size) benchs
    benchNames = map _.benchName benchs


getLegendItem :: Int -> String -> String
getLegendItem i name = 
  let
    color = fromMaybe "white" ( colors !! (i `mod` Array.length colors))
  in "![" <> color <> "](https://placehold.co/8x8/" <> color <> "/" <> color <> ".png) " <> name <> ""  
  
  -- "<span style=\"background-color: " <> color <> ";\">" <> name <> "</span>"  



-- - ![#f03c15](https://placehold.co/15x15/f03c15/f03c15.png) `#f03c15`
-- - ![#c5f015](https://placehold.co/15x15/c5f015/c5f015.png) `#c5f015`
-- - ![#1589F0](https://placehold.co/15x15/1589F0/1589F0.png) `#1589F0`

getLine' :: Array Size -> BenchResult -> String
getLine' sizes bench = "line [" <> (Str.joinWith ", " $ map Number.toString durs) <> "]"
  where
    mp = Map.fromFoldable $ map (\{size, duration: Milliseconds duration } -> size /\ duration) bench.samples
    durs = map (\size -> fromMaybe 0.0 $ Map.lookup size mp) sizes

getXTicks' ::  Array Size ->  Array BenchResult -> String
getXTicks' sizes benchs_ = fromMaybe "0" $ do
  sizes <- NEA.fromArray sizes
  pure $ Str.joinWith ", " $ map (Int.toStringAs Int.decimal) $ (getXTicks sizes)

getXTicks :: NonEmptyArray Int -> Array Int
getXTicks sizes =
  let
    head = NEA.head sizes
    last = NEA.last sizes
    step = foldr1 gcd sizes
  in
    unfoldr
      ( \st ->
          let
            next = st + step
          in
            if st > last then Nothing
            else Just (st /\ next)

      )
      head