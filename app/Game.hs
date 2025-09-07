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
import Data.Maybe (catMaybes)

data Block = Air | Dirt | Stone | Stairs | Fire
    deriving (Eq)

data DigRequest ph = DigRequest Dir Int (Index ph)

type AdjacentPair ph = (Index ph, Index ph)

type MovingPart ph =
    ( Int -- how many ticks should pass until thhe part drops by 1 block
    , AdjacentPair ph -- the bottom part of a collapsing column
    )

data Game board ph = Game
    { player :: (Index ph, Maybe (DigRequest ph))
    , board :: board
    , movingParts :: [MovingPart ph]
    }

type GameM ph m a =
    forall board.
    ( MBoard board m
    , MonadFail m
    , Item board ~ Block
    ) =>
    StateT (Game (board ph) ph) m a

trackDug ::
    Index ph ->
    GameM ph m ()
trackDug pos = do
    unlessM ((== Air) <$> blockTypeAt pos) $
        fail "Called dug on non-air!"

    whenJustM (pos `move` GoUp) $ \pair@(_, above) ->
        whenM (canFall <$> blockTypeAt above) $
            addToTracked (30, pair)
  where
    addToTracked i = State.modify' (\g@(Game _ _ movingParts) -> g{movingParts = i : movingParts})

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
    (Game (playerPos, digRequest) _ _) <- State.get
    let alreadyWantsToDigThere =
            -- if there's alread a dig request registered in that direction,
            -- we want to ignore it
            maybe False (\(DigRequest requestDir _ _) -> requestDir == dir) digRequest

    whenJustM (playerPos `move` dir) $
        unless alreadyWantsToDigThere
            . doMove
  where
    doMove (moveFrom, moveTo) = do
        nextBlockType <- blockTypeAt moveTo
        let needsToDig = nextBlockType == Dirt
        when needsToDig $
            State.modify' (\g -> g{player = (moveFrom, Just $ DigRequest dir 4 moveTo)})
        let willMove =
                not needsToDig
                    && nextBlockType /= Stone
                    && nextBlockType /= Fire
            needsStairs = do
                thisBlock <- blockTypeAt moveFrom
                pure $ dir == GoUp && thisBlock == Air

        whenM needsStairs $
            write' moveFrom Stairs
        when willMove $
            State.modify' (\g -> g{player = (moveTo, Nothing)})

update :: (State.MonadIO m) => GameM ph m ()
update = do
    updateMovingParts
    dropPlayerIfAir
    updateDigRequest

updateDigRequest :: GameM ph m ()
updateDigRequest = do
    g@(Game (playerPos, digRequest) _ _) <- State.get
    whenJust digRequest $ \(DigRequest dir ticks nextPos) ->
        case ticks of
            0 -> do
                write' nextPos Air
                State.put $
                    g
                        { player =
                            -- if we dug up, we stay here; if we dug down - we'll fall anyway,
                            -- so we only need to care for left and right
                            ( if dir == GoLeft || dir == GoRight then nextPos else playerPos
                            , -- dig request evaluated, reset it!
                              Nothing
                            )
                        }
                trackDug nextPos
            _ ->
                State.put $
                    g{player = (playerPos, Just (DigRequest dir (ticks - 1) nextPos))}

dropPlayerIfAir :: GameM ph m ()
dropPlayerIfAir =
    whenJustM belowPlayerM $ \belowPlayer ->
        whenM (isAir' belowPlayer) $
            State.modify' $
                \g -> g{player = (belowPlayer, Nothing)}
  where
    isAir' = fmap (Air ==) . blockTypeAt

    belowPlayerM :: GameM ph m (Maybe (Index ph))
    belowPlayerM = State.get >>= \(Game (playerPos, _) _ _) -> playerPos .> GoDown

updateMovingParts :: (State.MonadIO m) => GameM ph m ()
updateMovingParts = do
    g@(Game _ _ movingParts) <- State.get
    movingParts' <- mapM updateMovingPart movingParts
    State.put $ g{movingParts = catMaybes movingParts'}

updateMovingPart :: MovingPart ph -> GameM ph m (Maybe (MovingPart ph))
updateMovingPart (1, movingPart@(fallTo, _)) = do
    belowType <- blockTypeAt fallTo
    if belowType == Air || belowType == Stairs
        then do
            pullDownAt movingPart >>= \case
                Nothing -> do
                    explode
                    pure Nothing
                Just movingPart' ->
                    pure $ Just (5, movingPart')
        else pure Nothing
  where
    explode = mapMM_ explodeAt $ findExplosives fallTo
updateMovingPart (tick, movingPart) = pure $ Just (tick - 1, movingPart)

pullDownAt ::
    AdjacentPair ph ->
    GameM ph m (Maybe (AdjacentPair ph))
pullDownAt i = go i >> hitTheGround
  where
    hitTheGround =
        let isGround t = t /= Air && t /= Stairs
         in i `moveP` GoDown >>= \case
                Nothing -> pure Nothing
                Just pair@(belowBelow, _) ->
                    ifM
                        (isGround <$> blockTypeAt belowBelow)
                        (pure Nothing)
                        (pure $ Just pair)
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
    Index ph ->
    GameM ph m [Index ph]
findExplosives j = liftM2 (++) (searchAbove j) (searchBelow j)
  where
    -- i included!
    searchAbove i =
        let rest = i .> GoUp >>= ifJustM searchAbove
         in blockTypeAt i >>= \case
                Fire -> (i :) <$> rest
                Stone -> rest
                _ -> pure []
    -- i excluded!
    searchBelow i =
        i .> GoDown
            >>= ifJustM
                ( \jBelow ->
                    blockTypeAt jBelow >>= \case
                        Fire -> (jBelow :) <$> searchBelow jBelow
                        _ -> (pure [])
                )

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
