module Scene.Intro.Draw where

-- import Data.Maybe (fromJust)
-- import Text.Printf (printf)

-- import Apecs
import Apecs.Gloss
-- import Control.Lens hiding (set)
-- import Linear.V2 (V2(..))

-- import qualified Apecs as Entity

-- import Scene.Intro.Components
import World.Components (Foom(..), FoomStatus(..))
import Utils.Draw (textLines)
-- import Utils.Debug (Measure(..))
import World (SystemW)

draw :: SystemW Picture
draw = foldDraw $ \Foom{..} ->
  -- Measure{..} <- fromJust <$> cfold (\_ is -> Just is) Nothing
  let
    foomLines =
      "Force Operations" :
        if _foomStatus == Offline && _foomProgress < 50 then
          []
        else
          ["Ordnance Maintenance: " <> show _foomStatus]

    bootLine =
      "System diagnostics in progress... " <> show @Int (truncate _foomProgress) <> "%"

    progressLines =
      if _foomStatus == Booting then
        bootLine : take (truncate $ _foomProgress / 10)
          [ "TODO: boot message 1"
          , "TODO: boot message 2"
          , "TODO: boot message 3"
          , "TODO: boot message 4"
          , "TODO: boot message 5"
          , "TODO: boot message 6"
          , "TODO: boot message 7"
          , "Operator intervention required."
          , "Please stand by..."
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

    topMsg =
      translate (-400) 372 . scale 0.16 0.12 .
        color (withGreen 0.66 black) $
          textLines $ concat
            [ foomLines
            , progressLines
            , activationLines
            ]

    viewScreen =
      case _foomStatus of
        Activating ->
          color green $
            rectangleWire
              800
              (600 * sin (pi / 2 * _foomProgress / 100))
        _ ->
          mempty

    bottomMsg =
      translate (-400) (-312) . scale 0.16 0.12 .
        color cyan $
          textLines
            [ ""
            , "Press button to assume direct control."
            , ""
            , ""
            ]
  in
    mconcat
      [ topMsg
      , viewScreen
      , bottomMsg
      ]
