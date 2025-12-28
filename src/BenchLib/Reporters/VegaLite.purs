module BenchLib.Reporters.VegaLite
  ( Opts
  , reportVegaLite
  , reportVegaLite_
  ) where

import Prelude

import BenchLib (GroupResult, Reporter, defaultReporter)
import Data.Argonaut (stringifyWithIndent)
import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAP
import Data.Codec.Argonaut.Record as CAR
import Data.Foldable (maximum, minimum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as Str
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (catchException)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FS
import Node.Path (FilePath)

type Opts =
  { folderPath :: FilePath
  , indent :: Int
  , minTime :: Maybe Number
  , maxTime :: Maybe Number
  , minSize :: Maybe Int
  , maxSize :: Maybe Int
  }

defaultOpts :: Opts
defaultOpts =
  { folderPath: "bench-results-vega-lite"
  , indent: 2
  , minTime: Nothing
  , maxTime: Nothing
  , minSize: Nothing
  , maxSize: Nothing
  }

reportVegaLite_ :: Reporter
reportVegaLite_ = reportVegaLite identity

reportVegaLite :: (Opts -> Opts) -> Reporter
reportVegaLite mkOpts =
  let
    opts = mkOpts defaultOpts
  in
    defaultReporter
      { onSuiteStart = \_ -> liftEffect do
          -- Create the directory if it doesn't exist
          -- Try to create it, ignore error if it already exists
          catchException (\_ -> pure unit) $ FS.mkdir opts.folderPath
      , onGroupFinish = \groupResult -> liftEffect do
          let fileName = sanitizeFileName groupResult.groupName <> ".vl.json"
          let filePath = opts.folderPath <> "/" <> fileName
          let vegaLiteJson = CA.encode codecVegaLiteSpec $ toVegaLiteSpec opts groupResult
          FS.writeTextFile UTF8 filePath $ stringifyWithIndent opts.indent vegaLiteJson
          Console.log ("Wrote Vega-Lite report to " <> filePath)
      }

sanitizeFileName :: String -> String
sanitizeFileName = Str.replaceAll (Str.Pattern " ") (Str.Replacement "-")
  >>> Str.replaceAll (Str.Pattern "/") (Str.Replacement "_")
  >>> Str.replaceAll (Str.Pattern "\\") (Str.Replacement "_")

type VegaLiteSpec =
  { "$schema" :: String
  , data :: { values :: Array DataValue }
  , mark :: { type :: String, point :: Boolean }
  , encoding :: VegaLiteEncoding
  }

type VegaLiteEncoding =
  { x :: FieldEncoding
  , y :: FieldEncoding
  , color :: ColorEncoding
  }

type FieldEncoding =
  { field :: String
  , type :: String
  , title :: String
  , scale :: Maybe { domain :: Array Number }
  , axis :: Maybe { tickCount :: Int, values :: Maybe (Array Number) }
  }

type ColorEncoding =
  { field :: String
  , type :: String
  , legend :: Legend
  }

type Legend =
  { title :: String
  , orient :: String
  , symbolLimit :: Number
  }

type DataValue =
  { size :: Int
  , ms :: Number
  , series :: String
  }

toVegaLiteSpec :: Opts -> GroupResult -> VegaLiteSpec
toVegaLiteSpec opts { benchResults } =
  let
    dataValues = Array.concatMap
      ( \{ benchName, samples } ->
          map
            ( \{ size, average: Milliseconds ms } ->
                { size, ms, series: benchName }
            )
            samples
      )
      benchResults
    
    -- Calculate min/max from data if not provided
    sizes = map _.size dataValues
    times = map _.ms dataValues
    
    minSize' = fromMaybe (fromMaybe 0 $ minimum sizes) opts.minSize
    maxSize' = fromMaybe (fromMaybe 0 $ maximum sizes) opts.maxSize
    minTime' = fromMaybe (fromMaybe 0.0 $ minimum times) opts.minTime
    maxTime' = fromMaybe (fromMaybe 0.0 $ maximum times) opts.maxTime
    
    xScale = Just { domain: [ Int.toNumber minSize', Int.toNumber maxSize' ] }
    yScale = Just { domain: [ minTime', maxTime' ] }
    
    -- Use actual size values from data for ticks
    uniqueSizes = Array.sort $ Array.nub sizes
    xAxis = Just { tickCount: Array.length uniqueSizes, values: Just $ map Int.toNumber uniqueSizes }
    yAxis = Just { tickCount: 10, values: Nothing }
  in
    { "$schema": "https://vega.github.io/schema/vega-lite/v6.json"
    , data: { values: dataValues }
    , mark: { type: "line", point: true }
    , encoding:
        { x: { field: "size", type: "quantitative", title: "Size", scale: xScale, axis: xAxis }
        , y: { field: "ms", type: "quantitative", title: "Time (ms)", scale: yScale, axis: yAxis }
        , color:
            { field: "series"
            , type: "nominal"
            , legend: { title: "Implementation", orient: "right", symbolLimit: 30.0 }
            }
        }
    }

codecVegaLiteSpec :: JsonCodec VegaLiteSpec
codecVegaLiteSpec = CAR.object "VegaLiteSpec"
  { "$schema": CA.string
  , data: CAR.object "Data"
      { values: CA.array codecDataValue
      }
  , mark: CAR.object "Mark"
      { type: CA.string
      , point: CA.boolean
      }
  , encoding: codecEncoding
  }

codecDataValue :: JsonCodec DataValue
codecDataValue = CAR.object "DataValue"
  { size: CA.int
  , ms: CA.number
  , series: CA.string
  }

codecEncoding :: JsonCodec VegaLiteEncoding
codecEncoding = CAR.object "VegaLiteEncoding"
  { x: codecFieldEncoding
  , y: codecFieldEncoding
  , color: codecColorEncoding
  }

codecFieldEncoding :: JsonCodec FieldEncoding
codecFieldEncoding = CAR.object "FieldEncoding"
  { field: CA.string
  , type: CA.string
  , title: CA.string
  , scale: CAP.maybe $ CAR.object "Scale"
      { domain: CA.array CA.number
      }
  , axis: CAP.maybe $ CAR.object "Axis"
      { tickCount: CA.int
      , values: CAP.maybe $ CA.array CA.number
      }
  }

codecColorEncoding :: JsonCodec ColorEncoding
codecColorEncoding = CAR.object "ColorEncoding"
  { field: CA.string
  , type: CA.string
  , legend: codecLegend
  }

codecLegend :: JsonCodec Legend
codecLegend = CAR.object "Legend"
  { title: CA.string
  , orient: CA.string
  , symbolLimit: CA.number
  }

