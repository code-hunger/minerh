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
    whenJustM belowPlayerM $ \belowPlayer ->
        whenM (isAir belowPlayer ^&&^ (not <$> isOnStairs)) $ do
            fallingState' <- computeNewFallState belowPlayer
            State.modify' $
                \g -> g{player = (belowPlayer, fallingState')}
  where
    belowPlayerM :: GameM ph m (Maybe (Index ph))
    belowPlayerM = playerPos >>= (.> GoDown)

    isOnStairs :: GameM ph m Bool
    isOnStairs = (Stairs ==^) . blockTypeAt =<< playerPos

updatePlayerState :: forall m ph. GameM ph m ()
updatePlayerState = do
    g@(Game (playerPos_, playerState) _ _) <- State.get
    case playerState of
        Running dir nextPos ->
            let canBreatheAtNext = canBreathe <$> blockTypeAt nextPos
                stairsNeededAt = case dir of
                    GoUp -> playerPos_
                    GoDown -> nextPos
                    _ -> error "Running implemented only for up/down."
                canClimb = Stairs ==^ blockTypeAt stairsNeededAt
             in ifM
                    (canClimb ^&&^ canBreatheAtNext)
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
        Falling -> whenJustM (playerPos_ .> GoDown) $ \nextPos ->
            ifM
                (canStepOn <$> blockTypeAt nextPos)
                (State.put g{player = (playerPos_, Standing)})
                (State.put g{player = (nextPos, Falling)})
        _ -> pure ()

computeNewFallState :: Index ph -> GameM ph m (PlayerState ph)
computeNewFallState pos =
    ifM ((Stairs ==) <$> blockTypeAt pos) (pure Standing) $
        -- if we won't step on stairs (which are safe), we have to check what's below
        pos .> GoDown >>= \case
            Nothing -> logInfo "Tried to fall into the abyss! Kept it Standing though." >> pure Standing
            Just belowNext ->
                ifM
                    (isAir belowNext)
                    (pure Falling)
                    (pure Standing)

pullMovingPartsDown :: GameM ph m ([AdjacentPair ph], [MovingPart ph])
pullMovingPartsDown = do
    (Game _ _ movingParts) <- State.get
    partitionOutcomes <$> mapM updateMovingPart movingParts
  where
    partitionOutcomes :: [MovementOutcome ph] -> ([AdjacentPair ph], [MovingPart ph])
    partitionOutcomes = partitionEithers . mapMaybe convert

    convert OutOfBoard = Nothing
    convert (StillFlying nextPosition) = Just $ Right nextPosition
    convert (HitGround ground) = Just $ Left ground

data MovementOutcome ph = OutOfBoard | HitGround (AdjacentPair ph) | StillFlying {nextPosition :: MovingPart ph}

updateMovingPart :: MovingPart ph -> GameM ph m (MovementOutcome ph)
updateMovingPart (1, movingPart@(fallOn, _)) = do
    belowType <- blockTypeAt fallOn
    if isGround belowType
        then pure $ HitGround movingPart
        else pullDownAt movingPart
updateMovingPart (tick, movingPart) =
    pure $ StillFlying (tick - 1, movingPart)

isGround :: Block -> Bool
isGround t = t /= Air && t /= Stairs

canStepOn :: Block -> Bool
canStepOn t = t /= Air

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
                    (isGround <$> blockTypeAt belowBelow)
                    (pure $ HitGround pair)
                    (pure $ StillFlying{nextPosition = (10, pair)})
    go :: AdjacentPair ph -> GameM ph m ()
    go pair@(below, above) = do
        whenM (canFall <$> blockTypeAt above) $ do
            blockType <- blockTypeAt above
            write' above Air
            write' below blockType
            nextPair <- pair .> GoUp
            whenJust nextPair go

write' ::
    Index ph ->
    Block ->
    GameM ph m ()
write' i val = State.get >>= (\(Game _ b _) -> lift $ write b i val)

explodeAt ::
    Index ph ->
    GameM ph m ()
explodeAt = mapMM_ (`write'` Air) . neighbours

explodeGroundHits :: [AdjacentPair ph] -> GameM ph m ()
explodeGroundHits groundHits = do
    explosives <- concatMapM findExplosives groundHits
    mapM_ explodeAt explosives
    possibleFalls <- mapMaybeM justify' $ concat . for explosives $ \explosive ->
        let i = unIndex explosive
         in [Coord x' (y i - 1) | x' <- [x i - 1 .. x i + 1]]

    forM_ possibleFalls $ \i ->
        whenJustM
            (trackDug i)
            (addToTracked 5)

trackDug ::
    Index ph ->
    GameM ph m (Maybe (AdjacentPair ph))
trackDug pos = do
    unlessM (isAir pos) $
        fail "Called track dug on non-air!"

    (pos .> GoUp) >>= \case
        Nothing -> pure Nothing
        Just above ->
            ifM
                (canFall <$> blockTypeAt above)
                (pure (Just (pos, above)))
                (pure Nothing)

addToTracked :: Int -> AdjacentPair ph -> GameM ph m ()
addToTracked delay i = do
    logInfo $ "tracking " ++ show i ++ "\n"
    State.modify'
        (\g@(Game _ _ movingParts) -> g{movingParts = (delay, i) : movingParts})

logInfo :: String -> GameM ph m ()
logInfo = State.liftIO . appendFile "log"

findExplosives ::
    AdjacentPair ph ->
    GameM ph m [Index ph]
findExplosives (below, above) =
    ifM
        ((Fire ==) <$> blockTypeAt below)
        ((below :) <$> searchAbove above)
        (searchAbove above)
  where
    searchAbove :: Index ph -> GameM ph m [Index ph]
    searchAbove i =
        let rest = (i .> GoUp) >>= maybe (pure []) searchAbove
         in blockTypeAt i >>= \case
                Fire -> (i :) <$> rest
                Stone -> rest
                _ -> pure []
