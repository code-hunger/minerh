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

update :: (State.MonadIO m) => GameM ph m ()
update = do
    updateMovingParts
    dropPlayerIfAir
    playerState >>= updatePlayerState >>= setPlayerState

updateMovingParts :: (State.MonadIO m) => GameM ph m ()
updateMovingParts = do
    (groundsHit, pulledDown) <- pullMovingPartsDown

    State.modify' (\g -> g{movingParts = pulledDown})

    explodeGroundHits groundsHit

dropPlayerIfAir :: GameM ph m ()
dropPlayerIfAir =
    whenJustM (playerPosM ^> GoDown) $ \belowPlayer ->
        whenM (belowPlayer .~ Air ^&&^ notOnStairs) $
            computeNewFallState belowPlayer >>= setPlayerState
  where
    notOnStairs :: GameM ph m Bool
    notOnStairs = not <$> playerPosM ^~ Stairs

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
    (Digging dir ticks (playerPos_, nextPos)) ->
        case ticks of
            0 -> do
                write' nextPos Air
                fallingState' <- computeNewFallState nextPos
                whenJustM (trackDug nextPos) (addToTracked 60)
                pure $
                    -- if we dug up, we stay here; if we dug down - we'll fall anyway,
                    -- so we only need to care for left and right
                    if dir == GoUp
                        then Standing playerPos_
                        else fallingState'
            _ ->
                pure $ Digging dir (ticks - 1) (playerPos_, nextPos)
    Falling (playerPos_, nextPos) ->
        ifM
            (nextPos !~ Air)
            (pure $ Standing playerPos_)
            ( (nextPos .> GoDown) <&> \case
                Just nextNextPos -> Falling (nextPos, nextNextPos)
                Nothing -> Standing nextPos
            )
    other -> pure other

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
newtype GroundHit ph = GroundHit (AdjacentPair ph)

pullMovingPartsDown :: GameM ph m ([GroundHit ph], [MovingPart ph])
pullMovingPartsDown = do
    (Game _ _ movingParts) <- State.get
    partitionOutcomes <$> mapM updateMovingPart movingParts
  where
    partitionOutcomes :: [MovementOutcome ph] -> ([GroundHit ph], [MovingPart ph])
    partitionOutcomes = partitionEithers . mapMaybe convert

    convert OutOfBoard = Nothing
    convert (StillFlying nextPosition) = Just $ Right nextPosition
    convert (HitGround ground) = Just $ Left ground

    updateMovingPart :: MovingPart ph -> GameM ph m (MovementOutcome ph)
    updateMovingPart (1, movingPart@(fallOn, _)) =
        ifM
            (fallOn .~ Ground)
            (pure . HitGround $ GroundHit movingPart)
            (pullDownAt movingPart)
    updateMovingPart (tick, movingPart) =
        pure $ StillFlying (tick - 1, movingPart)

    pullDownAt ::
        AdjacentPair ph ->
        GameM ph m (MovementOutcome ph)
    pullDownAt i = go i >> hitTheGround
      where
        hitTheGround =
            i .> GoDown >>= \case
                Nothing -> pure OutOfBoard
                Just pair@(belowBelow, _) ->
                    ifM
                        (belowBelow .~ Ground)
                        (pure $ HitGround $ GroundHit pair)
                        (pure $ StillFlying{nextPosition = (10, pair)})
        go :: AdjacentPair ph -> GameM ph m ()
        go pair@(below, above) = do
            whenM (above .~ Heavy) $ do
                blockType <- blockTypeAt above
                write' above Air
                write' below blockType
                nextPair <- pair .> GoUp
                whenJust nextPair go

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
addToTracked delay i = do
    logInfo $ "tracking " ++ show i ++ "\n"
    State.modify'
        (\g@(Game _ _ movingParts) -> g{movingParts = (delay, i) : movingParts})

explodeGroundHits :: [GroundHit ph] -> GameM ph m ()
explodeGroundHits groundHits = do
    explosives <- concatMapM findExplosives groundHits
    mapM_ explode explosives
    possibleFalls <- mapMaybeM justify' $ concat . for explosives $ \explosive ->
        let i = unIndex explosive
         in [Coord x' (y i - 1) | x' <- [x i - 1 .. x i + 1]]

    forM_ possibleFalls $ \i ->
        whenJustM
            (trackDug i)
            (addToTracked 5)
  where
    findExplosives ::
        GroundHit ph ->
        GameM ph m [Index ph]
    findExplosives (GroundHit (below, above)) =
        ifM
            (below .~ Fire)
            ((below :) <$> searchAbove above)
            (searchAbove above)

    searchAbove :: Index ph -> GameM ph m [Index ph]
    searchAbove i =
        let rest = (i .> GoUp) >>= maybe (pure []) searchAbove
         in blockTypeAt i >>= \case
                Fire -> (i :) <$> rest
                Stone -> rest
                _ -> pure []

explode ::
    Index ph ->
    GameM ph m ()
explode = mapMM_ (`write'` Air) . neighbours

logInfo :: String -> GameM ph m ()
logInfo = State.liftIO . appendFile "log"
