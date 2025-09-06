{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Game where

import Board (Board (Item, justify, (!)), Coord (..), Index (unIndex), MBoard (write))
import Control.Monad.Extra
import Control.Monad.State (MonadTrans (lift), StateT)
import qualified Control.Monad.State as State
import Data.Maybe (catMaybes)

data Block = Air | Dirt | Stone | Stairs | Fire
    deriving (Eq)

data DigRequest ph = DigRequest Dir Int (Index ph)

data Game board ph = Game
    { player :: (Index ph, Maybe (DigRequest ph))
    , board :: board
    , movingParts ::
        [ ( Int -- how many ticks should pass until thhe part drops by 1 block
          , Index ph -- the bottom part of a collapsing column
          , Index ph -- the piece below it
          )
        ]
    }

trackDug :: forall m b ph. (MBoard b m, MonadFail m, Item b ~ Block) => Index ph -> StateT (Game (b ph) ph) m ()
trackDug pos = do
    unlessM ((== Air) <$> blockTypeAt pos) $
        fail "Called dug on non-air!"

    pos .> GoUp >>= \case
        Nothing -> pure ()
        Just above ->
            whenM (canFall <$> blockTypeAt above) $
                addToTracked (30, above, pos)
  where
    addToTracked i = State.modify' (\g@(Game _ _ movingParts) -> g{movingParts = i : movingParts})

blockTypeAt ::
    ( MBoard board m
    ) =>
    Index ph ->
    StateT (Game (board ph) ph) m (Item board)
blockTypeAt i = State.get >>= (\(Game _ b _) -> lift $ b ! i)

canFall :: Block -> Bool
canFall blockType = blockType == Stone || blockType == Fire

(.>) ::
    ( MBoard board m
    ) =>
    Index ph ->
    Dir ->
    StateT (Game (board ph) ph) m (Maybe (Index ph))
i .> dir = justify' (movePos' (unIndex i) dir)

justify' ::
    (MBoard board m) =>
    Coord ->
    StateT (Game (board ph) ph) m (Maybe (Index ph))
justify' i =
    State.get >>= \(Game _ board _) -> lift $ justify board i

moveUp' :: Coord -> Coord
moveUp' = (`movePos'` GoUp)

movePlayer ::
    ( MBoard board m
    , Item board ~ Block
    , MonadFail m
    ) =>
    Dir ->
    StateT (Game (board ph) ph) m ()
movePlayer dir = do
    (Game (pos, digRequest) board _) <- State.get
    pos .> dir >>= \case
        Nothing -> pure ()
        Just nextPos -> do
            let alreadyWantsToDigThere =
                    maybe False (\(DigRequest requestDir _ _) -> requestDir == dir) digRequest

            when (not alreadyWantsToDigThere) $ do
                nextBlock <- lift $ board ! nextPos
                thisBlock <- lift $ board ! pos
                let needsToDig = nextBlock == Dirt
                when needsToDig $
                    State.modify' (\g -> g{player = (pos, Just $ DigRequest dir 4 nextPos)})
                let willMove =
                        not needsToDig
                            && nextBlock /= Stone
                            && nextBlock /= Fire
                let needsStairs = dir == GoUp && thisBlock == Air && willMove
                when needsStairs $
                    lift $
                        write board pos Stairs
                when willMove $
                    State.modify' (\g -> g{player = (nextPos, Nothing)})

update ::
    forall m board ph.
    (MBoard board m, Item board ~ Block, MonadFail m) =>
    StateT (Game (board ph) ph) m ()
update = do
    (Game (playerPos, digRequest) board movingParts) <- State.get
    movingParts' <- fmap concat $ forM movingParts $ \(tick, fallFrom, fallTo) -> do
        belowType <- blockTypeAt fallTo
        if belowType == Air || belowType == Stairs
            then
                if tick > 1
                    then pure [(tick - 1, fallFrom, fallTo)]
                    else do
                        pullDownAt fallFrom fallTo >>= \case
                            Nothing -> do
                                mapM_ explodeAt =<< findExplosives fallTo
                                pure []
                            Just iBelowBelow ->
                                pure [(5, fallTo, iBelowBelow)]
            else pure []
    State.modify' (\g -> g{movingParts = movingParts'})

    let isAir' = fmap (Air ==) . blockTypeAt
    let dropPlayerIfAir belowPlayer =
            whenM (isAir' belowPlayer) $
                State.modify' $
                    \g -> g{player = (belowPlayer, Nothing)}
    playerPos .> GoDown >>= maybe (pure ()) dropPlayerIfAir

    whenJust digRequest $ \(DigRequest dir ticks nextPos) ->
        if ticks > 0
            then State.modify' (\g -> g{player = (playerPos, Just (DigRequest dir (ticks - 1) nextPos))})
            else do
                lift (write board nextPos Air)
                trackDug nextPos
                State.modify'
                    ( \g ->
                        g
                            { player =
                                -- if we dug up, we stay here; if we dug down - we'll fall anyway,
                                -- so we only need to care for left and right
                                ( if dir == GoLeft || dir == GoRight then nextPos else playerPos
                                , -- dig request evaluated, reset it!
                                  Nothing
                                )
                            }
                    )

pullDownAt ::
    forall m board ph.
    (MBoard board m, Item board ~ Block) =>
    Index ph ->
    Index ph ->
    StateT (Game (board ph) ph) m (Maybe (Index ph))
pullDownAt i iBelow = go i iBelow >> hitTheGround
  where
    hitTheGround =
        let isGround t = t /= Air && t /= Stairs
         in iBelow .> GoDown >>= \case
                Nothing -> pure Nothing
                Just iBelowBelow ->
                    ifM
                        (isGround <$> blockTypeAt iBelowBelow)
                        (pure Nothing)
                        (pure $ Just iBelowBelow)
    go j jBelow = do
        whenM (canFall <$> blockTypeAt j) $ do
            blockType <- blockTypeAt j
            write' j Air
            write' jBelow blockType
            j .> GoUp >>= \case
                Nothing -> pure ()
                Just aboveJ -> go aboveJ j

write' ::
    (MBoard board m) =>
    Index ph ->
    Item board ->
    StateT (Game (board ph) ph) m ()
write' i val = State.get >>= (\(Game _ b _) -> lift $ write b i val)

explodeAt ::
    (MBoard board m, Item board ~ Block) =>
    Index ph ->
    StateT (Game (board ph) ph) m ()
explodeAt (unIndex -> i) = mapM_ blow =<< neighbours
  where
    blow j = write' j Air
    neighbours = fmap catMaybes . mapM justify' $ [Coord x' y' | x' <- [x i - 1 .. x i + 1], y' <- [y i - 1 .. y i + 1]]

findExplosives ::
    (MBoard board m, Item board ~ Block) =>
    Index ph ->
    StateT (Game (board ph) ph) m [Index ph]
findExplosives j = liftM2 (++) (searchAbove j) (searchBelow j)
  where
    searchAbove i = do
        i .> GoUp >>= \case
            Nothing -> pure []
            Just i' -> do
                blockType <- blockTypeAt i
                case blockType of
                    Fire -> (i :) <$> searchAbove i'
                    Stone -> searchAbove i'
                    _ -> pure []
    searchBelow i =
        i .> GoDown
            >>= maybe
                (pure [])
                ( \jBelow ->
                    ifM
                        ((== Fire) <$> blockTypeAt jBelow)
                        ((jBelow :) <$> searchBelow jBelow)
                        (pure [])
                )

data Dir = GoLeft | GoRight | GoUp | GoDown
    deriving (Eq, Show)

movePos' :: Coord -> Dir -> Coord
movePos' c GoLeft = Coord (x c - 1) (y c)
movePos' c GoRight = Coord (x c + 1) (y c)
movePos' c GoUp = Coord (x c) (y c - 1)
movePos' c GoDown = Coord (x c) (y c + 1)
