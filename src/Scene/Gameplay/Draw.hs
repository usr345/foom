module Scene.Gameplay.Draw where

import Apecs (global, cfold, cfoldM)
import Apecs.Gloss
import Control.Lens hiding (set)
import Data.List (sort)
import Data.Traversable (for)
import Linear.Affine (distanceA)
import Linear.Metric (normalize)
import Linear.V2 (V2(..))
import Linear.Vector ((^/), (^*))
import Text.Printf (printf)

import qualified Apecs as Entity

import Utils.Draw (textLines)
import World.Components
import World (World, SystemW)

draw :: SystemW Picture
draw = do
  terrain <- drawTerrain

  cities <- foldDraw drawCity
  silos <- foldDraw drawSilo

  intercepts <- foldDraw drawIntercept
  missiles <- foldDraw drawMissile
  mirvs <- foldDraw drawMirv

  blasts <- foldDraw drawBlast

  tracer <- drawTracer
  tracker <- drawTracker
  cursor <- foldDraw drawCursor
  topMsg <- foldDraw drawTopMsg
  score <- foldDraw drawScore

  let
    scene = translate 0 (-200) $ mconcat
      [ intercepts
      , missiles
      , mirvs
      , terrain
      , cities
      , silos
      , blasts
      ]

    ui = mconcat
      [ color red $ rectangleWire 800 600
      , topMsg
      , score
      , tracer
      , tracker
      , cursor
      ]

  pure $ scene <> ui

drawTerrain :: SystemW Picture
drawTerrain = do
  score <- Entity.get global
  let
    hits = fromIntegral $ score ^. groundHits
    greyish = greyN . max 0.1 $ 1 - sqrt hits / 10
    brown = makeColorI 139 69 19 255

    groundColor = if
      | hits == 0 ->
          green
      | hits <= 15 ->
          mixColors (1.0 - hits / 15) (hits / 15) green brown
      | hits <= 30 ->
          mixColors (1.0 - (hits - 15) / 15) ((hits - 15) / 15) brown greyish
      | otherwise ->
          greyish

  pure $
    translate 0 (-50) .
      color groundColor $
        rectangleSolid 800 100

drawCity :: (City, Position) -> Picture
drawCity (City ruined, Position (V2 px py)) =
  translate px py .
    color col $
      rectangleSolid 30 10
  where
    col =
      if ruined then
        greyN 0.25
      else
        kindaBlue

    kindaBlue = makeColor 0.2 0.3 1.0 1.0

drawSilo :: (Silo, Position) -> Picture
drawSilo (Silo ammo, Position (V2 px py)) =
  translate px py $ mconcat
    [ color col $
        rectangleSolid 30 10
    , color cyan $
        translate (-40) 30 . scale 0.33 0.17 $
          text (show ammo)
    ]
  where
    col =
      if ammo > 0 then
        red
      else
        darkRed

    darkRed = makeColor 0.3 0 0 1.0

drawIntercept :: (Intercept, Position) -> Picture
drawIntercept (i, Position pos) =
  mconcat
    [ drawTrail blue o pos
    , drawProjectile cyan pos
    , drawTarget
    ]
  where
    drawTarget =
      translate tx ty .
        color (withAlpha 0.7 cyan) $ mconcat
          [ line [(2, 2), (4, 4)]
          , line [(-2, 2), (-4, 4)]
          , line [(2, -2), (4, -4)]
          , line [(-2, -2), (-4, -4)]
          ]

    Position o = i ^. interceptOrigin
    Position (V2 tx ty) = i ^. interceptTarget

drawMissile :: (Missile, Position) -> Picture
drawMissile (m, Position pos) =
  mconcat
    [ drawTrail (withAlpha 0.5 red) o pos
    , drawProjectile orange pos
    ]
  where
    o = m ^. missileOrigin . _Position

drawMirv :: (MIRV, Position) -> Picture
drawMirv (m, Position pos) =
  mconcat
    [ drawTrail (withAlpha 0.5 red) o pos
    , drawProjectile orange pos
    ]
  where
    o = m ^. mirvOrigin . _Position

drawTrail :: Color -> V2 Float -> V2 Float -> Picture
drawTrail c (V2 ox oy) (V2 px py) =
  color c $
    line [ (ox, oy), (px, py) ]

drawProjectile :: Color -> V2 Float -> Picture
drawProjectile c (V2 px py) =
  translate px py . -- rotate phi .
    color c $
      circle 2

drawBlast :: (Blast, Position) -> Picture
drawBlast (b, Position (V2 px py)) =
  translate px py .
    color col $
      circleSolid r
  where
    (col, r) = case b ^. blastPhase of
      BlastGrowing ->
        ( white
        , b ^. blastTimer * 100
        )
      BlastBurning ->
        ( yellow
        , 20 + (b ^. blastTimer) * 4
        )
      BlastSmoking ->
        ( greyN $ 1 - b ^. blastTimer
        , max 0 (1 - b ^. blastTimer) * 20
        )

drawTracer :: SystemW Picture
drawTracer =
  cfor_ $ \Foom{_foomStatus} ->
    if _foomStatus > Calibrating then
      cfor_ $ \(Cursor, Position dst) ->
        if (insideUI dst) then do
          let V2 dx dy = dst

          armed <- flip cfold [] $ \acc (Silo ammo, Position pos) ->
            if ammo > 0 then
              (distanceA dst pos, pos - V2 0 200) : acc
            else
              acc

          case sort armed of
            [] ->
              pure mempty
            (_dist, V2 sx sy) : _ ->
              pure .
                color (makeColor 0 0.5 0.5 0.5) $
                  line
                    [ (sx, sy)
                    , (dx, dy)
                    ]
        else
          pure mempty
      else
        pure mempty

cfor_
  :: _ -- (Entity.Has World IO c, Monoid a)
  => (c -> SystemW a) -> SystemW a
cfor_ proc = cfoldM (const proc) mempty

drawTracker :: SystemW Picture
drawTracker =
  cfor_ $ \Foom{_foomStatus} ->
    if _foomStatus > Assessment then do
      missiles <- flip cfold [] $ \acc (m, Position pos, Velocity vel) ->
        ( distanceA (m ^. missileTarget . _Position) pos / 75
        , pos
        , vel
        ) : acc

      armed <- flip cfold [] $ \acc (Silo ammo, Position pos) ->
        if ammo > 0 then
          pos - V2 0 200 : acc
        else
          acc

      let tracked = 1
      fmap mconcat . for (take tracked $ sort missiles) $ \(hitIn, tpos, vel) -> do
        let
          V2 tx ty = tpos - V2 0 200
          fpos@(V2 fx fy) = tpos - V2 0 200 + vel
          closest = sort $ do
            spos <- armed
            pure (distanceA fpos spos / 250, spos)

          tracks = mconcat $ do
            (intIn, V2 _sx _sy) <- take 1 closest
            let V2 ix iy = normalize vel ^* (intIn * 75)
            pure $
              color (withAlpha 0.5 cyan) .
                translate (tx + ix) (ty + iy) $
                  circle (intIn * 10)
                -- XXX: debug tracing
                -- line [ (sx, sy), (tx + ix, ty + iy) ] <>
                -- (translate fx fy $
                --   scale 0.2 0.2 .
                --     text $ printf "%0.1f" intIn
                -- )
        pure tracks
        -- XXX: debug tracing
        -- let V2 vx vy = vel
        -- pure . mappend tracks $
        --   translate tx ty $
        --     color (withAlpha 0.5 orange) $
        --       line [ (0, 0), (vx, vy) ] <>
        --       scale 0.2 0.2 (text $ printf "%.1f" (hitIn + 1))
    else
      pure mempty

drawCursor :: (Cursor, Position) -> Picture
drawCursor (_, Position cur@(V2 curX curY)) =
  if insideUI cur then
    translate curX curY .
      color red $
        circle 4
  else
    mempty

drawTopMsg :: Foom -> Picture
drawTopMsg Foom{..} =
  translate (-400) 376 . scale 0.16 0.12 .
    color (withGreen 0.66 black) $
      textLines
        $ "Force Operations"
        : "Ordnance Management: " <> status
        : foomMessages
  where
    status = unwords
      [ show _foomStatus
      , if showProgress then
          printf "%.1f%%" _foomProgress
        else
          ""
      ]

    showProgress = _foomStatus `elem` [Calibrating, Assessment]

    foomMessages =
      case _foomStatus of
        Recovering ->
          [ "Operator connection confirmed."
          , "Manual fire control operational."
          ]

        Calibrating ->
          "Updating targeting database with interceptor profiles." :
          [ "New device connected: guidance lasers. Updating firmware..."
          | _foomProgress >= 50
          ]

        Assessment ->
          if
            | _foomProgress <= 25 ->
                [ "Threat assessment in progress."
                ]
            | _foomProgress <= 50 ->
                [ "Threat assessment in progress."
                , "Updating delivery scheduler."
                ]
            | _foomProgress <= 75 ->
                [ "Updating delivery scheduler."
                , "Operator inadequacy suspected."
                ]
            | otherwise ->
                [ ""
                , "I sense damage."
                ]

        Ready ->
          [ "Running at full capacity."
          , "Tactical services available, operator confirmation required."
          ]

        s ->
          error $ "assert: not a gameplay status: " <> show s

drawScore :: Score -> Picture
drawScore Score{..} = mconcat
  [ translate (-400) (-340) .
      scale 0.25 0.25 .
        color cyan .
          text $ "Score: " <> show _interceptorHits
  , translate 0 (-340) .
      scale 0.25 0.25 .
        color magenta .
          text $ "Casualties: " <> show _cityHits <> "m"
  ]
