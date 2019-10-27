module Scene.Gameplay.Tick where

import Control.Monad (when, void)
import Control.Monad.Reader (liftIO)
import Data.Foldable (for_)
import Data.Proxy (Proxy(..))
import System.Random (randomRIO)

import Apecs (Not(..), cfold, cmap, cmapM_, global, newEntity)
import Control.Lens ((&), (^.), (.~), (+~))
import Linear.V2 (V2(..))
import Linear.Affine (distanceA)
import Linear.Metric (normalize)

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
    if distanceA (i ^. interceptTarget . _Position) pos <= 10 then
      Left
        ( Blast BlastGrowing 0
        , Velocity $ vel / 5
        , Not @Intercept
        )
    else
      Right ()

interceptorHit :: SystemW ()
interceptorHit =
  cmapM_ $ \(b, Position bp) -> do
    cmapM_ $ \(Missile{}, Position mp, missile) ->
      if distanceA mp bp <= 20 + (b ^. blastTimer) * 4 then do
        Entity.destroy missile $ Proxy @(Missile, Position, Velocity)
        cmap $ interceptorHits +~ 1
        cmap $ siloStockpile +~ 1
      else
        pure ()

    cmapM_ $ \(MIRV{}, Position mp, mirv) ->
      if distanceA mp bp <= 20 + (b ^. blastTimer) * 4 then do
        Entity.destroy mirv $ Proxy @(MIRV, Position, Velocity)
        cmap $ interceptorHits +~ 1
        cmap $ siloStockpile +~ 1
      else
        pure ()

launchMissiles :: SystemW ()
launchMissiles = do
  count <- cfold (\n Missile{} -> n + 1) (0 :: Int)
  when (count < 10) $ do
    dice <- liftIO $ randomRIO @Int (0, 60 * 2)
    when (dice == 0) $ do
      ox <- liftIO $ randomRIO (-400, 400)
      let o = V2 ox 300 + V2 0 200

      tx <- liftIO $ randomRIO (-400, 400)
      let t = V2 tx 0

      void $ newEntity
        ( Missile
            { _missileOrigin = Position o
            , _missileTarget = Position t
            }
        , Position o
        , Velocity $ normalize (t - o) * 75
        )

launchMIRV :: SystemW ()
launchMIRV = do
  count <- cfold (\n MIRV{} -> n + 1) (0 :: Int)
  when (count < 1) $ do
    dice <- liftIO $ randomRIO @Int (0, 60 * 5)
    when (dice == 0) $ do
      ox <- liftIO $ randomRIO (-400, 400)
      let o@(V2 _ oy) = V2 ox 300 + V2 0 200

      tx <- liftIO $ randomRIO (-400, 400)
      let t = V2 tx 0

      fuse <- liftIO $ randomRIO (0.33, 0.66)
      let m = V2 (ox * fuse + tx * (1 - fuse)) (oy * fuse)

      void $ newEntity
        ( MIRV
            { _mirvOrigin = Position o
            , _mirvSplit  = Position m
            , _mirvTarget = Position t
            }
        , Position o
        , Velocity $ normalize (t - o) * 66
        )

missileHit :: SystemW ()
missileHit =
  cmapM_ $ \(Missile{}, Position mp@(V2 _mx my), m) ->
    when (my <= 0) $ do
      Entity.modify global $ groundHits +~ 1

      cmapM_ $ \(City ruined, Position cp, c) ->
        when (not ruined && distanceA mp cp <= 35) $ do
          Entity.modify global $ cityHits +~ 1
          Entity.modify c $ cityRuined .~ True

      cmapM_ $ \(Silo ammo, Position sp, s) ->
        when (distanceA mp sp <= 35) $ do
          Entity.modify global $ siloHits +~ 1
          Entity.modify s $ siloStockpile .~
            max 0 (truncate @Float $ fromIntegral ammo / 2)

      Entity.destroy m $ Proxy @Missile
      Entity.set m $ Blast BlastGrowing 0
      Entity.set m $ Velocity $ V2 0 25

mirvSplitup :: SystemW ()
mirvSplitup =
  cmapM_ $ \(m, Position mp, mirv) -> do
    let ms = m ^. mirvSplit . _Position
    when (distanceA mp ms < 10) $ do
      Entity.destroy mirv $ Proxy @(MIRV, Position, Velocity)

      let o = m ^. mirvSplit . _Position
      let V2 tx ty = m ^. mirvTarget . _Position

      for_ [-1 .. 1] $ \n -> do
        let t = V2 (max (-350) . min 350 $ tx + n * 50) ty

        void $ newEntity
          ( Missile
              { _missileOrigin = Position o
              , _missileTarget = Position t
              }
          , Position mp
          , Velocity $ normalize (t - o) * 75
          )
