module Scene.Intro.Draw where

-- import Data.Maybe (fromJust)
-- import Text.Printf (printf)

-- import Apecs
import Apecs.Gloss
-- import Control.Lens hiding (set)
import Linear.V2 (V2(..))

-- import qualified Apecs as Entity

import Scene.Intro.Components (HoloRay(..), HoloScreen(..))
-- import Utils.Debug (Measure(..))
import Utils.Draw (textLines)
import World (SystemW)
import World.Components (Foom(..), FoomStatus(..))

draw :: SystemW Picture
draw = do
  foom <- foldDraw drawFoom
  rays <- foldDraw drawRays
  screen <- foldDraw drawScreen
  pure $ mconcat
    [ foom
    , rays
    , screen
    ]

drawScreen :: HoloScreen -> Picture
drawScreen HoloScreen{..} =
  if _hsOpen > 0 then
    translate tx ty .
      color _hsColor $
        rectangleWire
          sizeX
          (sizeY * sin (pi / 2 * _hsOpen))
  else
    mempty
  where
    V2 tx ty = _hsPos
    V2 sizeX sizeY = _hsSize

drawFoom :: Foom -> Picture
drawFoom Foom{..} =
  mconcat
    [ topMsg
    , bottomMsg
    ]
  where
    topMsg =
      translate (-400) 376 . scale 0.16 0.12 .
        color (withGreen 0.66 black) $
          textLines $ concat
            [ foomLines
            , progressLines
            , activationLines
            ]
      where
        foomLines =
          "Force Operations" :
            if _foomStatus == Offline && _foomProgress < 50 then
              []
            else
              ["Ordnance Maintenance: " <> show _foomStatus]

        bootLine = concat
          [ "System diagnostics in progress... "
          , show @Int (truncate _foomProgress)
          , "%"
          ]

        progressLines =
          if _foomStatus == Booting then
            bootLine : take (truncate $ _foomProgress / 10)
              [ "[INFO] Hardware modules authenticated."
              , "[INFO] Signature chains verified."
              , "[INFO] Security domains established."
              , "[INFO] Sensor network online."
              , "[WARN] Preliminary threat assessment: RED ALERT."
              , "[ERROR] Calibration data corrupted."
              , "[INFO] Emergency modelling routine starting."
              , "[CRITICAL] Operator intervention required."
              , "[NOTICE] Please stand by..."
              ]
          else
            []

        activationLines =
          if _foomStatus == Activating then
            [ "Activating direct control... " <> show @Int (truncate _foomProgress) <> "%"
            ] <>
            [ "God have mercy on our souls."
            | _foomProgress > 90
            ]
          else
            []

    bottomMsg =
      translate (-400) (-312) . scale 0.16 0.12 .
        color cyan $
          textLines
            [ ""
            , "Press button to assume direct control."
            , ""
            , ""
            ]

drawRays :: HoloRay -> Picture
drawRays HoloRay{..} = mconcat
  [ color (withAlpha _hrOpacity _hrColor) $
      line
        [ (sx, sy)
        , (ex, ey)
        ]
  ]
  where
    V2 sx sy = _hrOrigin
    V2 ex ey = _hrTarget
