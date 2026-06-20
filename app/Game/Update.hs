{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Game.Update (update, write', computeNewFallState) where

import Control.Monad.State.Strict (MonadTrans (lift))
import qualified Control.Monad.State.Strict as State
import Data.Either (partitionEithers)
import Data.Functor ((<&>))

import Board (Coord (..), Index (unIndex), MBoard (write))
import BoardGen (mapMM_)
import Game.Core

import Control.Monad.Extra
import Data.Maybe (mapMaybe)
import Data.Traversable (for)

update :: GameM ph m ()
update = do
    pullMovingPartsDown
    playerState >>= updatePlayerState >>= setPlayerState

updatePlayerState :: forall m ph. PlayerState ph -> GameM ph m (PlayerState ph)
updatePlayerState = \case
    Running dir (playerPos_, nextPos) ->
        let stairsNeededAt = case dir of
                GoUp -> playerPos_
                GoDown -> nextPos
                _ -> error "Running implemented only for up/down."
            canClimb = stairsNeededAt .~ Stairs
         in ifM
                (canClimb ^&&^ canBreatheAt nextPos)
                ( nextPos .> dir <&> \case
                    Just nextNextPos ->
                        Running dir (nextPos, nextNextPos)
                    Nothing ->
                        Standing nextPos
                )
                (pure $ Standing playerPos_)
    Digging dir 0 (playerPos_, nextPos) -> do
        write' nextPos Air
        fallingState' <- computeNewFallState nextPos
        whenJustM (trackDug nextPos) (addToTracked 60)
        pure $
            if dir == GoUp
                then Standing playerPos_
                else fallingState'
    Digging dir ticks (playerPos_, nextPos) ->
        pure $ Digging dir (ticks - 1) (playerPos_, nextPos)
    Falling (playerPos_, nextPos) ->
        ifM
            (nextPos !~ Air)
            (pure $ Standing playerPos_)
            ( (nextPos .> GoDown) <&> \case
                Just nextNextPos -> Falling (nextPos, nextNextPos)
                Nothing -> Standing nextPos
            )
    Standing playerPos_ ->
        (playerPos_ .> GoDown) >>= \case
            Just belowPlayer ->
                ifM
                    (belowPlayer .~ Air ^&&^ notOnStairs)
                    (computeNewFallState belowPlayer)
                    (pure $ Standing playerPos_)
            Nothing -> pure $ Standing playerPos_
  where
    notOnStairs :: GameM ph m Bool
    notOnStairs = not <$> playerPosM ^~ Stairs

computeNewFallState :: Index ph -> GameM ph m (PlayerState ph)
computeNewFallState pos =
    ifM
        (pos .~ Stairs)
        (pure $ Standing pos)
        $ pos .> GoDown >>= \case
            -- if we won't step on stairs (which are safe), we have to check what's below
            Nothing -> logInfo "Tried to fall into the abyss! Kept it Standing though.\n" >> pure (Standing pos)
            Just belowNext ->
                ifM
                    (belowNext .~ Air)
                    (pure $ Falling (pos, belowNext))
                    (pure $ Standing pos)

-- A hitting of the ground is a pair of locations (below, top) denoting that the top one hit the
-- bottom as a result of a fall.
data GroundHit ph = GroundHit (Index ph) [Index ph]

pullMovingPartsDown :: GameM ph m ()
pullMovingPartsDown = do
    (Game _ _ movingParts) <- State.get

    (groundsHit, pulledDown) <- partitionOutcomes <$> mapM updateMovingPart movingParts

    State.modify' (\g -> g{movingParts = pulledDown})

    explodeGroundHits groundsHit
  where
    partitionOutcomes :: [MovementOutcome ph] -> ([GroundHit ph], [MovingPart ph])
    partitionOutcomes = partitionEithers . mapMaybe convert

    convert OutOfBoard = Nothing
    convert (StillFlying nextPosition) = Just $ Right nextPosition
    convert (HitGround ground) = Just $ Left ground

    updateMovingPart :: MovingPart ph -> GameM ph m (MovementOutcome ph)
    updateMovingPart (1, fallOn, movingPart) =
        ifM (fallOn .~ Ground) (pure . HitGround $ GroundHit fallOn movingPart) doPullDown
      where
        doPullDown = do
            movingPart `pullDownTo` fallOn
            (fallOn, movingPart) .> GoDown >>= \case
                Nothing -> pure OutOfBoard
                Just (belowFall, column) ->
                    ifM
                        (belowFall .~ Ground)
                        (pure $ HitGround $ GroundHit belowFall column)
                        (pure $ StillFlying{nextPosition = (10, belowFall, column)})
    updateMovingPart (tick, height, movingPart) =
        pure $ StillFlying (tick - 1, height, movingPart)

    [] `pullDownTo` fallOn = write' fallOn Air
    (above : rest) `pullDownTo` fallOn = do
        blockType <- blockTypeAt above
        write' fallOn blockType
        rest `pullDownTo` above

data MovementOutcome ph = OutOfBoard | HitGround (GroundHit ph) | StillFlying {nextPosition :: MovingPart ph}

write' ::
    Index ph ->
    Block ->
    GameM ph m ()
write' i val = State.get >>= (\(Game _ b _) -> lift $ write b i val)

trackDug ::
    Index ph ->
    GameM ph m (Maybe (AdjacentPair ph))
trackDug pos = do
    unlessM (pos .~ Air) $
        error "Called track dug on non-air!"

    (pos .> GoUp) >>= \case
        Nothing -> pure Nothing
        Just above ->
            ifM
                (above .~ Heavy)
                (pure (Just (pos, above)))
                (pure Nothing)

addToTracked :: Int -> AdjacentPair ph -> GameM ph m ()
addToTracked delay (below, above) = do
    logInfo $ "tracking " ++ show below ++ "\n"
    fallingColumn <- go above
    State.modify'
        (\g@(Game _ _ movingParts) -> g{movingParts = (delay, below, fallingColumn) : movingParts})
  where
    go i =
        ifM
            (i .~ Heavy)
            ( (i .> GoUp) >>= \case
                Just next -> (i :) <$> go next
                Nothing -> pure []
            )
            (pure [])

explodeGroundHits :: [GroundHit ph] -> GameM ph m ()
explodeGroundHits groundHits = do
    explosives <- concatMapM findExplosives groundHits
    go explosives
  where
    findExplosives (GroundHit below above) = do
        explosiveBelow <- ifM (below .~ Fire) (pure [below]) (pure [])
        explosivesAbove <- filterM (.~ Fire) above
        pure (explosiveBelow ++ explosivesAbove)
    go [] = pure ()
    go explosives = do
        (remainingFires, explodedIndices) <- runExplosion explosives

        forM_ explodedIndices $ \i ->
            whenJustM (trackDug i) (addToTracked 5)

        go remainingFires

-- explodes the neighbours of the fires which are not fires themselves, and returns the fires.
-- The idea is that any fires neighbouring the explosion will explode on their own a bit later.
runExplosion ::
    [Index ph] ->
    GameM ph m ([Index ph], [Index ph])
runExplosion is = do
    -- TODO: WRONG! This may break moving parts which are tracked!
    forM_ is (`write'` Air)
    (firesAround, rest) <- partitionM (.~ Fire) =<< concatMapM neighbours is
    forM_ rest (`write'` Air)
    pure (firesAround, rest)

logInfo :: String -> GameM ph m ()
logInfo = State.liftIO . appendFile "log"
