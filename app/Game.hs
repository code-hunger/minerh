{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Game where

import Board (Board (Item, justify, (!)), Coord (..), Index (unIndex), MBoard (write))
import Control.Monad.Extra
import Control.Monad.State (MonadTrans (lift), StateT)
import qualified Control.Monad.State as State
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Maybe (catMaybes)

data Block = Air | Dirt | Stone | Stairs | Fire
    deriving (Eq)

data DigRequest ph = DigRequest Dir Int (Index ph)

data Game board ph = Game
    { player :: (Index ph, Maybe (DigRequest ph))
    , board :: board
    , movingParts ::
        [ ( Int -- how many ticks should pass until thhe part drops by 1 block
          , AdjacentPair ph -- the bottom part of a collapsing column
          )
        ]
    }

type GameM m board ph a =
    ( MBoard board m
    , MonadFail m
    , Item board ~ Block
    ) =>
    StateT (Game (board ph) ph) m a

trackDug ::
    Index ph ->
    GameM m board ph ()
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
    GameM m board ph (Item board)
blockTypeAt i = State.get >>= (\(Game _ board _) -> lift $ board ! i)

canFall :: Block -> Bool
canFall blockType = blockType == Stone || blockType == Fire

(.>) ::
    Index ph ->
    Dir ->
    GameM m board ph (Maybe (Index ph))
i .> dir = justify' (movePos' (unIndex i) dir)

justify' ::
    Coord ->
    GameM m board ph (Maybe (Index ph))
justify' i =
    State.get >>= \(Game _ board _) -> lift $ justify board i

moveUp' :: Coord -> Coord
moveUp' = (`movePos'` GoUp)

type AdjacentPair ph = (Index ph, Index ph)

move ::
    Index ph ->
    Dir ->
    GameM m board ph (Maybe (Index ph, Index ph))
i `move` dir =
    i .> dir >>= \case
        Nothing -> pure Nothing
        Just j -> pure $ Just (i, j)

moveP ::
    AdjacentPair ph ->
    Dir ->
    GameM m board ph (Maybe (AdjacentPair ph))
(i, j) `moveP` dir = do
    runMaybeT $ do
        i' <- MaybeT $ i .> dir
        j' <- MaybeT $ j .> dir
        pure (i', j')

movePlayer :: Dir -> GameM m board ph ()
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

update :: GameM m board ph ()
update = do
    (Game (playerPos, digRequest) board movingParts) <- State.get
    movingParts' <- fmap concat $ forM movingParts $ \(tick, movingPart) -> do
        let (fallTo, _) = movingPart
        belowType <- blockTypeAt fallTo
        if belowType == Air || belowType == Stairs
            then
                if tick > 1
                    then pure [(tick - 1, movingPart)]
                    else do
                        pullDownAt movingPart >>= \case
                            Nothing -> do
                                mapM_ explodeAt =<< findExplosives fallTo
                                pure []
                            Just movingPart' ->
                                pure [(5, movingPart')]
            else pure []
    State.modify' (\g -> g{movingParts = movingParts'})

    let isAir' = fmap (Air ==) . blockTypeAt
    let dropPlayerIfAir belowPlayer =
            whenM (isAir' belowPlayer) $
                State.modify' $
                    \g -> g{player = (belowPlayer, Nothing)}
    whenJustM (playerPos .> GoDown) dropPlayerIfAir

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
    AdjacentPair ph ->
    GameM m board ph (Maybe (AdjacentPair ph))
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
    Item board ->
    GameM m board ph ()
write' i val = State.get >>= (\(Game _ b _) -> lift $ write b i val)

explodeAt ::
    Index ph ->
    GameM m board ph ()
explodeAt (unIndex -> i) = mapM_ blow =<< neighbours
  where
    blow j = write' j Air
    neighbours = fmap catMaybes . mapM justify' $ [Coord x' y' | x' <- [x i - 1 .. x i + 1], y' <- [y i - 1 .. y i + 1]]

findExplosives ::
    Index ph ->
    GameM m board ph [Index ph]
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
