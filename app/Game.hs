{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Game where

import Board (Board (Item, justify, (!)), Coord (..), Index (unIndex), MBoard (write))
import BoardGen (mapMM_)
import Control.Monad.Extra
import Control.Monad.State (MonadTrans (lift), StateT)
import qualified Control.Monad.State as State
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, mapMaybe)
import Data.Traversable (for)

data Block = Air | Dirt | Stone | Stairs | Fire
    deriving (Eq)

data DigRequest ph = DigRequest Dir Int (Index ph)

type AdjacentPair ph = (Index ph, Index ph)

type MovingPart ph =
    ( Int -- how many ticks should pass until thhe part drops by 1 block
    , AdjacentPair ph -- the bottom part of a collapsing column
    )

data PlayerFallingState = Standing | Falling deriving (Eq)

data Game board ph = Game
    { player ::
        ( Index ph
        , PlayerFallingState
        , Maybe (DigRequest ph)
        )
    , board :: board
    , movingParts :: [MovingPart ph]
    }

type GameM ph m a =
    forall board.
    ( MBoard board m
    , State.MonadIO m -- we just use this for logging
    , MonadFail m
    , Item board ~ Block
    ) =>
    StateT (Game (board ph) ph) m a

trackDug ::
    Index ph ->
    GameM ph m (Maybe (AdjacentPair ph))
trackDug pos = do
    unlessM ((== Air) <$> blockTypeAt pos) $
        fail "Called track dug on non-air!"

    (pos .> GoUp) >>= \case
        Nothing -> pure Nothing
        Just above ->
            ifM
                (canFall <$> blockTypeAt above)
                (pure (Just (pos, above)))
                (pure Nothing)

logInfo :: String -> GameM ph m ()
logInfo = State.liftIO . appendFile "log"

addToTracked :: Int -> AdjacentPair ph -> GameM ph m ()
addToTracked delay i = do
    logInfo $ "tracking " ++ show i ++ "\n"
    State.modify'
        (\g@(Game _ _ movingParts) -> g{movingParts = (delay, i) : movingParts})

blockTypeAt ::
    Index ph ->
    GameM ph m Block
blockTypeAt i = State.get >>= (\(Game _ board _) -> lift $ board ! i)

canFall :: Block -> Bool
canFall blockType = blockType == Stone || blockType == Fire

(.>) ::
    Index ph ->
    Dir ->
    GameM ph m (Maybe (Index ph))
i .> dir = justify' (movePos' (unIndex i) dir)

justify' ::
    Coord ->
    GameM ph m (Maybe (Index ph))
justify' i =
    State.get >>= \(Game _ board _) -> lift $ justify board i

moveUp' :: Coord -> Coord
moveUp' = (`movePos'` GoUp)

move ::
    Index ph ->
    Dir ->
    GameM ph m (Maybe (Index ph, Index ph))
i `move` dir =
    i .> dir >>= \case
        Nothing -> pure Nothing
        Just j -> pure $ Just (i, j)

moveP ::
    AdjacentPair ph ->
    Dir ->
    GameM ph m (Maybe (AdjacentPair ph))
(i, j) `moveP` dir = do
    runMaybeT $ do
        i' <- MaybeT $ i .> dir
        j' <- MaybeT $ j .> dir
        pure (i', j')

movePlayer :: Dir -> GameM ph m ()
movePlayer dir = do
    (Game (playerPos, fallingState, digRequest) _ _) <- State.get
    let alreadyWantsToDigThere =
            -- if there's alread a dig request registered in that direction,
            -- we want to ignore it
            maybe False (\(DigRequest requestDir _ _) -> requestDir == dir) digRequest

    when (fallingState == Standing) $
        whenJustM (playerPos `move` dir) $
            unless alreadyWantsToDigThere
                . doMove
  where
    doMove :: AdjacentPair ph -> GameM ph m ()
    doMove (moveFrom, moveTo) = do
        nextBlockType <- blockTypeAt moveTo
        let needsToDig = nextBlockType == Dirt
        when needsToDig $
            State.modify' (\g -> g{player = (moveFrom, Standing, Just $ DigRequest dir 4 moveTo)})
        let willMove =
                not needsToDig
                    && nextBlockType /= Stone
                    && nextBlockType /= Fire
            needsStairs = do
                thisBlock <- blockTypeAt moveFrom
                pure $ dir == GoUp && thisBlock == Air

        whenM needsStairs $
            write' moveFrom Stairs
        when willMove $ do
            newFallState <- computeNewFallState moveTo
            State.modify' (\g -> g{player = (moveTo, newFallState, Nothing)})

computeNewFallState :: Index ph -> GameM ph m PlayerFallingState
computeNewFallState pos =
    pos .> GoDown >>= \case
        Nothing -> error "Fell into the abyss!"
        Just belowNext ->
            ifM
                ((Air ==) <$> blockTypeAt belowNext)
                (pure Falling)
                (pure Standing)

update :: (State.MonadIO m) => GameM ph m ()
update = do
    updateMovingParts
    dropPlayerIfAir
    updateDigRequest

updateDigRequest :: GameM ph m ()
updateDigRequest = do
    g@(Game (playerPos, fallingState, digRequest) _ _) <- State.get
    whenJust digRequest $ \(DigRequest dir ticks nextPos) ->
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
                                else playerPos
                            , fallingState'
                            , -- dig request evaluated, reset it!
                              Nothing
                            )
                        }
                whenJustM (trackDug nextPos) (addToTracked 30)
            _ ->
                State.put $
                    g{player = (playerPos, fallingState, Just (DigRequest dir (ticks - 1) nextPos))}

dropPlayerIfAir :: GameM ph m ()
dropPlayerIfAir =
    whenJustM belowPlayerM $ \belowPlayer ->
        whenM (isAir' belowPlayer) $ do
            newFallState <- computeNewFallState belowPlayer
            State.modify' $
                \g -> g{player = (belowPlayer, newFallState, Nothing)}
  where
    isAir' = fmap (Air ==) . blockTypeAt

    belowPlayerM :: GameM ph m (Maybe (Index ph))
    belowPlayerM = State.get >>= \(Game (playerPos, _, _) _ _) -> playerPos .> GoDown

updateMovingParts :: (State.MonadIO m) => GameM ph m ()
updateMovingParts = do
    (groundsHit, pulledDown) <- pullMovingPartsDown

    State.modify' (\g -> g{movingParts = pulledDown})

    explodeGroundHits groundsHit

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

pullDownAt ::
    AdjacentPair ph ->
    GameM ph m (MovementOutcome ph)
pullDownAt i = go i >> hitTheGround
  where
    hitTheGround =
        i `moveP` GoDown >>= \case
            Nothing -> pure OutOfBoard
            Just pair@(belowBelow, _) ->
                ifM
                    (isGround <$> blockTypeAt belowBelow)
                    (pure $ HitGround pair)
                    (pure $ StillFlying{nextPosition = (5, pair)})
    go pair@(below, above) = do
        whenM (canFall <$> blockTypeAt above) $ do
            blockType <- blockTypeAt above
            write' above Air
            write' below blockType
            pair `moveP` GoUp >>= ifJustM go

write' ::
    Index ph ->
    Block ->
    GameM ph m ()
write' i val = State.get >>= (\(Game _ b _) -> lift $ write b i val)

explodeAt ::
    Index ph ->
    GameM ph m ()
explodeAt = mapMM_ (`write'` Air) . neighbours

neighbours :: Index ph -> GameM ph m [Index ph]
neighbours (unIndex -> i) =
    fmap catMaybes
        . mapM justify'
        $ [Coord x' y' | x' <- [x i - 1 .. x i + 1], y' <- [y i - 1 .. y i + 1]]

findExplosives ::
    AdjacentPair ph ->
    GameM ph m [Index ph]
findExplosives (below, above) =
    ifM
        ((Fire ==) <$> blockTypeAt below)
        ((below :) <$> searchAbove above)
        (searchAbove above)
  where
    searchAbove i =
        let rest = i .> GoUp >>= ifJustM searchAbove
         in blockTypeAt i >>= \case
                Fire -> (i :) <$> rest
                Stone -> rest
                _ -> pure []

ifJustM ::
    forall a f t.
    (Applicative f, Monoid a) =>
    (t -> f a) ->
    Maybe t ->
    f a
ifJustM f (Just a) = f a
ifJustM _ Nothing = pure mempty

data Dir = GoLeft | GoRight | GoUp | GoDown
    deriving (Eq, Show)

movePos' :: Coord -> Dir -> Coord
movePos' c GoLeft = Coord (x c - 1) (y c)
movePos' c GoRight = Coord (x c + 1) (y c)
movePos' c GoUp = Coord (x c) (y c - 1)
movePos' c GoDown = Coord (x c) (y c + 1)
