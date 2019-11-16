module Scene.Gameplay.Tick where

import Control.Monad (when, void)
import Control.Monad.Reader (liftIO)
import Data.Foldable (for_)
import Data.Proxy (Proxy(..))
import System.Random (randomRIO)

import Apecs (Not(..), global, newEntity, ($~))
import Apecs.System (cfold, cmap, cmapIf, cmapM_)
import Control.Lens ((&), (^.), (.~), (+~))
import Linear.V2 (V2(..))
import Linear ((^*))
import Linear.Affine (distanceA)
import Linear.Metric (normalize)
import Config

import qualified Apecs as Entity

import World.Components
import World (SystemW)

onTick :: Float -> SystemW ()
onTick dt = do
  stepBlast dt

  interceptorHit
  interceptorBlast

  launchMissiles
  missileHit

  launchMIRV
  mirvSplitup

stepBlast :: Float -> SystemW ()
stepBlast dt = cmap $ \(blastTimer +~ dt -> b) ->
  case b ^. blastPhase of
    BlastGrowing ->
      Right $
        if b ^. blastTimer >= 0.2 then
          b & blastPhase .~ BlastBurning
            & blastTimer .~ 0
        else
          b
    BlastBurning ->
      Right $
        if b ^. blastTimer >= 0.2 then
          b & blastPhase .~ BlastSmoking
            & blastTimer .~ 0
        else
          b
    BlastSmoking ->
      if b ^. blastTimer >= 0.2 then
        Left $ Not @(Blast, Position, Velocity)
      else
        Right b

interceptorBlast :: SystemW ()
interceptorBlast =
  cmap $ \(i, Position pos, Velocity vel) ->
    -- XXX: can overshoot on laggy frames
    if distanceA (i ^. interceptTarget . _Position) pos <= (Config.interceptDistance configuration) then
      Left
        ( Blast BlastGrowing 0
        , Velocity $ vel / 5
        , Not @Intercept
        )
    else
      Right ()

interceptorHit :: SystemW ()
interceptorHit = do
  cmapM_ $ \(b, Position bp) -> do
    cmapM_ $ \(Missile{}, Position mp, missile) -> do
      let
        hitDist = distanceA mp bp - 20 + (b ^. blastTimer) * 4
        accuracy =
          if hitDist <= 0 then
            1 - hitDist / 20
          else
            0
      if accuracy > 0 then do
        Entity.destroy missile $ Proxy @(Missile, Position, Velocity)
        cmap $ interceptorHits +~ 1
        cmap $ siloStockpile +~ 1
        cmapIf ((== Calibrating) . _foomStatus) $
          foomProgress +~ accuracy
      else
        pure ()

    cmapM_ $ \(MIRV{}, Position mp, mirv) -> do
      let
        hitDist = distanceA mp bp - 20 + (b ^. blastTimer) * 4
        accuracy =
          if hitDist <= 0 then
            1 - hitDist / 20
          else
            0
      if accuracy > 0 then do
        Entity.destroy mirv $ Proxy @(MIRV, Position, Velocity)
        cmap $ interceptorHits +~ 1
        cmap $ siloStockpile +~ 1
        cmapIf ((== Calibrating) . _foomStatus) $
          foomProgress +~ accuracy
      else
        pure ()

  cmapM_ $ \Score{_interceptorHits} ->
    cmapM_ $ \(Foom{..}, foom) ->
      case _foomStatus of
        Recovering ->
          when (_interceptorHits >= 2) $ do
            foom $~ foomStatus .~ Calibrating
            foom $~ foomProgress .~ 0.0
        Calibrating  ->
          when (_foomProgress >= 100) $ do
            foom $~ foomStatus .~ Assessment
            foom $~ foomProgress .~ 0.0
        _ ->
          pure ()

-- velocityOfObject ::

getVelocity :: V2 Float -> V2 Float -> Float -> Velocity
getVelocity t o speed = Velocity $ normalize (t - o) ^* speed

launchMissiles :: SystemW ()
launchMissiles = do
  count <- cfold (\n Missile{} -> n + 1) (0 :: Int)
  when (count < (Config._MissileLimit configuration)) $ do
    dice <- liftIO $ randomRIO @Int (0, (Config.chanceMissile configuration))
    when (dice == 0) $ do
      ox <- randomUIx 0
      let o = V2 ox (Config.verticalSpan configuration) + V2 0 (Config.groundLevelShift configuration)

      tx <- randomUIx 0
      let t = V2 tx 0

      void $ newEntity
        ( Missile
            { _missileOrigin = Position o
            , _missileTarget = Position t
            }
        , Position o
        , getVelocity t o (Config.speedMissile configuration)
        )

randomUIx :: Float -> SystemW Float
randomUIx limit = liftIO $ randomRIO (-horSpan, horSpan)
  where
    horSpan = Config.horizontalSpan configuration - limit

launchMIRV :: SystemW ()
launchMIRV = do
  count <- cfold (\n MIRV{} -> n + 1) (0 :: Int)
  when (count < (Config._MIRVLimit configuration)) $ do
    dice <- liftIO $ randomRIO @Int (0, (Config.chanceMIRV configuration))
    when (dice == 0) $ do
      ox <- randomUIx 0
      let o@(V2 _ oy) = V2 ox 300 + V2 0 (Config.groundLevelShift configuration)

      nHeads <- liftIO $ randomRIO ((Config._MIRVMinHeads configuration), (Config._MIRVMaxHeads configuration))
      spread <- liftIO $ randomRIO ((Config._MIRVMinSpread configuration), (Config._MIRVMaxSpread configuration))

      tx <- randomUIx (fromIntegral nHeads * spread)
      let t = V2 tx 0

      fuse <- liftIO $ randomRIO (0.33, 0.66)
      let m = V2 (ox * fuse + tx * (1 - fuse)) (oy * fuse)

      void $ newEntity
        ( MIRV
            { _mirvOrigin = Position o
            , _mirvSplit  = Position m
            , _mirvTarget = Position t
            , _mirvSpread = spread * fromIntegral nHeads
            , _mirvHeads = nHeads
            }
        , Position o
        , getVelocity t o (Config.speedMIRV configuration)
        )

missileHit :: SystemW ()
missileHit =
  cmapM_ $ \(Missile{}, Position mp@(V2 _mx my), m) ->
    when (my <= 0) $ do
      Entity.modify global $ groundHits +~ 1
      cmapIf ((== Assessment) . _foomStatus) $
        foomProgress +~ 0.5

      cmapM_ $ \(City ruined, Position cp, c) -> do
        let
          hitDist = distanceA mp cp
          accuracy =
            if hitDist <= (Config.hitDist configuration) then
              (Config.hitDist configuration) - hitDist
            else
              0

        when (not ruined && accuracy > 0) $ do
          Entity.modify global $ cityHits +~ 1
          Entity.modify c $ cityRuined .~ True
          cmapIf ((== Assessment) . _foomStatus) $
            foomProgress +~ accuracy

      cmapM_ $ \(Silo ammo, Position sp, s) -> do
        let
          hitDist = distanceA mp sp
          accuracy =
            if hitDist <= (Config.hitDist configuration) then
              (Config.hitDist configuration) - hitDist
            else
              0

        when (accuracy > 0) $ do
          Entity.modify global $ siloHits +~ 1
          Entity.modify s $ siloStockpile .~
            max 0 (truncate @Float $ fromIntegral ammo / 2)
          cmapIf ((== Assessment) . _foomStatus) $
            foomProgress +~ accuracy

      Entity.destroy m $ Proxy @Missile
      Entity.set m $ Blast BlastGrowing 0
      Entity.set m $ Velocity $ V2 0 25

      cmapM_ $ \(Foom{..}, foom) ->
        when (_foomProgress >= 100) $
          case _foomStatus of
            Assessment -> do
              foom $~ foomStatus .~ Ready
              foom $~ foomProgress .~ 0.0
            _ ->
              pure ()

mirvRetarget :: MIRV -> [Position]
mirvRetarget mirv = do
    i <- [0..n-1]
    let rx = c + (2*s*i / (n - 1))
    pure $ Position (V2 rx ty)
  where
    V2 tx ty = mirv ^. mirvTarget . _Position
    s = mirv ^. mirvSpread
    c = tx - s
    n :: Float
    n = fromIntegral $ mirv ^. mirvHeads


mirvSplitup :: SystemW ()
mirvSplitup =
  cmapM_ $ \(m, Position mp, mirv) -> do
    let ms = m ^. mirvSplit . _Position
    when (distanceA mp ms < 10) $ do
      Entity.destroy mirv $ Proxy @(MIRV, Position, Velocity)

      let o = m ^. mirvSplit . _Position

      for_ (mirvRetarget m) $ \(Position t) ->

        void $ newEntity
          ( Missile
              { _missileOrigin = Position o
              , _missileTarget = Position t
              }
          , Position mp
          , getVelocity t o (Config.speedMissile configuration)
          )
