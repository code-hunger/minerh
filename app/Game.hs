{-# LANGUAGE TypeOperators #-}

module Game where

import Board (Board (Item, hasIndex, (!)), Coord (..), MBoard (write))
import Control.Monad.Extra (filterM, forM, forM_, unlessM, when, whenJust, whenM, (&&^))
import Control.Monad.State (MonadTrans (lift), StateT)
import qualified Control.Monad.State as State
import System.IO.Error (alreadyExistsErrorType)

data Block = Air | Dirt | Stone | Stairs | Fire
    deriving (Eq)

data DigRequest = DigRequest Dir Int

data Game board = Game
    { player :: (Coord, Maybe DigRequest)
    , board :: board
    , movingParts ::
        [ ( Int -- how many ticks should pass until thhe part drops by 1 block
          , Coord -- the bottom part of a collapsing column
          )
        ]
    }

trackDug :: forall m b. (MBoard b m, MonadFail m, Item b ~ Block) => Coord -> StateT (Game b) m ()
trackDug pos = do
    unlessM ((== Air) <$> blockTypeAt pos) $
        fail "Called dug on non-air!"

    let above = moveUp pos
    whenM
        ( hasIndex' above
            &&^ (canFall <$> blockTypeAt above)
        )
        $ addToTracked (30, above)
  where
    addToTracked i = State.modify' (\g -> g{movingParts = i : movingParts g})

hasIndex' i = State.get >>= (\(Game _ b _) -> lift $ hasIndex b i)
blockTypeAt i = State.get >>= (\(Game _ b _) -> lift $ b ! i)

canFall :: Block -> Bool
canFall blockType = blockType == Stone || blockType == Fire

moveUp :: Coord -> Coord
moveUp = (`movePos` GoUp)

movePlayer ::
    ( MBoard board m
    , Item board ~ Block
    , MonadFail m
    ) =>
    Dir ->
    StateT (Game board) m ()
movePlayer dir = do
    (Game (pos, digRequest) board _) <- State.get
    let nextPos = movePos pos dir
    inBounds <- lift $ hasIndex board nextPos

    let alreadyWantsToDigThere =
            maybe False (\(DigRequest requestDir _) -> requestDir == dir) digRequest

    when (inBounds && not alreadyWantsToDigThere) $ do
        nextBlock <- lift $ board ! nextPos
        thisBlock <- lift $ board ! pos
        let needsToDig = nextBlock == Dirt
        when needsToDig $
            State.modify' (\g -> g{player = (pos, Just $ DigRequest dir 4)})
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
    forall m board.
    (MBoard board m, Item board ~ Block, MonadFail m) =>
    StateT (Game board) m ()
update = do
    (Game (playerPos, digRequest) board movingParts) <- State.get
    movingParts' <- fmap concat $ forM movingParts $ \(tick, i) -> do
        let below = movePos i GoDown
        belowType <- lift $ board ! below
        if belowType == Air || belowType == Stairs
            then
                if tick > 1
                    then pure [(tick - 1, i)]
                    else do
                        hitTheGround <- collapseAt i
                        if hitTheGround
                            then do
                                mapM_ explodeAt =<< lift (findExplosions board (i `movePos` GoDown))
                                pure []
                            else
                                pure [(5, below)]
            else pure []
    State.modify' (\g -> g{movingParts = movingParts'})

    let belowPlayer = movePos playerPos GoDown
    belowPlayerType <- lift $ board ! belowPlayer
    when (belowPlayerType == Air) $ do
        State.modify' $ \g -> g{player = (belowPlayer, Nothing)}

    whenJust digRequest $ \(DigRequest dir ticks) ->
        if ticks > 0
            then State.modify' (\g -> g{player = (playerPos, Just (DigRequest dir (ticks - 1)))})
            else do
                let nextPos = playerPos `movePos` dir
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

collapseAt ::
    forall m board.
    (MBoard board m, Item board ~ Block) =>
    Coord ->
    StateT (Game board) m Bool
collapseAt i = go i >> hitTheGround
  where
    hitTheGround =
        let below = i `movePos` GoDown `movePos` GoDown
            isGround t = t /= Air && t /= Stairs
         in isGround <$> blockTypeAt below
    go j = do
        whenM (hasIndex' j &&^ (canFall <$> blockTypeAt j)) $ do
            blockType <- blockTypeAt j
            write' j Air
            write' (movePos j GoDown) blockType
            go (moveUp j)

write' i x = State.get >>= (\(Game _ b _) -> lift $ write b i x)

explodeAt ::
    (MBoard board m, Item board ~ Block) =>
    Coord ->
    StateT (Game board) m ()
explodeAt (Coord x y) = mapM_ blow =<< neighbours
  where
    blow i = write' i Air
    neighbours = filterM hasIndex' [Coord x' y' | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1]]

findExplosions ::
    (MBoard board m, Item board ~ Block) =>
    board ->
    Coord ->
    m [Coord]
findExplosions board i = (++) <$> go GoUp i <*> go GoDown i
  where
    go dir i = do
        isValid <- hasIndex board i
        let i' = i `movePos` dir
        if isValid
            then do
                blockType <- board ! i
                case blockType of
                    Fire -> (i :) <$> go dir i'
                    Stone -> go dir i'
                    _ -> pure []
            else pure []

data Dir = GoLeft | GoRight | GoUp | GoDown
    deriving (Eq, Show)

movePos :: Coord -> Dir -> Coord
movePos (Coord x y) GoLeft = Coord (x - 1) y
movePos (Coord x y) GoRight = Coord (x + 1) y
movePos (Coord x y) GoUp = Coord x (y - 1)
movePos (Coord x y) GoDown = Coord x (y + 1)
