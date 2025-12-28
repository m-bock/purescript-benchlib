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
import Data.Codec.Argonaut.Record as CAR
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
  }

defaultOpts :: Opts
defaultOpts =
  { folderPath: "bench-results-vega-lite"
  , indent: 2
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
          let vegaLiteJson = CA.encode codecVegaLiteSpec $ toVegaLiteSpec groupResult
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

toVegaLiteSpec :: GroupResult -> VegaLiteSpec
toVegaLiteSpec { benchResults } =
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
  in
    { "$schema": "https://vega.github.io/schema/vega-lite/v6.json"
    , data: { values: dataValues }
    , mark: { type: "line", point: false }
    , encoding:
        { x: { field: "size", type: "quantitative", title: "Size" }
        , y: { field: "ms", type: "quantitative", title: "Time (ms)" }
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

