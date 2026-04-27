{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Game.Update (update, write', computeNewFallState) where

import Control.Monad.State.Strict (MonadTrans (lift), StateT)
import qualified Control.Monad.State.Strict as State
import Data.Either (partitionEithers)

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
    updatePlayerState

updateMovingParts :: (State.MonadIO m) => GameM ph m ()
updateMovingParts = do
    (groundsHit, pulledDown) <- pullMovingPartsDown

    State.modify' (\g -> g{movingParts = pulledDown})

    explodeGroundHits groundsHit

dropPlayerIfAir :: GameM ph m ()
dropPlayerIfAir =
    whenJustM (playerPos ^> GoDown) $ \belowPlayer ->
        whenM (belowPlayer .~ Air ^&&^ notOnStairs) $ do
            fallingState' <- computeNewFallState belowPlayer
            State.modify' $
                \g -> g{player = (belowPlayer, fallingState')}
  where
    notOnStairs :: GameM ph m Bool
    notOnStairs = not <$> playerPos ^~ Stairs

updatePlayerState :: forall m ph. GameM ph m ()
updatePlayerState = do
    g@(Game (playerPos_, playerState) _ _) <- State.get
    case playerState of
        Running dir nextPos ->
            let stairsNeededAt = case dir of
                    GoUp -> playerPos_
                    GoDown -> nextPos
                    _ -> error "Running implemented only for up/down."
                canClimb = stairsNeededAt .~ Stairs
             in ifM
                    (canClimb ^&&^ canBreatheAt nextPos)
                    ( nextPos .> dir >>= \case
                        Just nextNextPos ->
                            State.put $ g{player = (nextPos, Running dir nextNextPos)}
                        Nothing ->
                            State.put $ g{player = (nextPos, Standing)}
                    )
                    (State.put $ g{player = (playerPos_, Standing)})
        (Digging dir ticks nextPos) ->
            case ticks of
                0 -> do
                    write' nextPos Air
                    fallingState' <- computeNewFallState nextPos
                    State.put $
                        g
                            { player =
                                -- if we dug up, we stay here; if we dug down - we'll fall anyway,
                                -- so we only need to care for left and right
                                ( if dir == GoLeft || dir == GoRight
                                    then nextPos
                                    else playerPos_
                                , fallingState'
                                )
                            }
                    whenJustM (trackDug nextPos) (addToTracked 60)
                _ ->
                    State.put $
                        g{player = (playerPos_, Digging dir (ticks - 1) nextPos)}
        Falling ->
            playerPos_ .> GoDown >>= \case
                Just nextPos ->
                    ifM
                        (nextPos !~ Air)
                        (State.put g{player = (playerPos_, Standing)})
                        (State.put g{player = (nextPos, Falling)})
                Nothing ->
                    do
                        logInfo "Was falling, but hit the ground\n"
                        State.put g{player = (playerPos_, Standing)}
        _ -> pure ()

computeNewFallState :: Index ph -> GameM ph m (PlayerState ph)
computeNewFallState pos =
    ifM
        (pos .~ Stairs)
        (pure Standing)
        $ pos .> GoDown >>= \case
            -- if we won't step on stairs (which are safe), we have to check what's below
            Nothing -> logInfo "Tried to fall into the abyss! Kept it Standing though." >> pure Standing
            Just belowNext ->
                ifM
                    (belowNext .~ Air)
                    (pure Falling)
                    (pure Standing)

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
        fail "Called track dug on non-air!"

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
