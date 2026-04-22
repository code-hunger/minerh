{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Game.Core where

import Board (Board (Item, justify, (!)), Coord (..), Index (unIndex), MBoard)

import Control.Monad.Extra (liftM2)
import Control.Monad.State.Strict (MonadTrans (lift), StateT)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Maybe (catMaybes)

data Block = Air | Dirt | Stone | Stairs | Fire
    deriving (Eq)

data Dir = GoLeft | GoRight | GoUp | GoDown
    deriving (Eq, Show, Read)

type AdjacentPair ph = (Index ph, Index ph)

type MovingPart ph =
    ( Int -- how many ticks should pass until thhe part drops by 1 block
    , AdjacentPair ph -- the bottom part of a collapsing column
    )

data PlayerState ph
    = Standing
    | Digging Dir Int (Index ph)
    | Falling
    | Running Dir (Index ph)
    deriving (Show, Read)

data Game board ph = Game
    { player :: (Index ph, PlayerState ph)
    , board :: board
    , movingParts :: [MovingPart ph]
    }

-- The GameM monad is a state monad (with IO for logging) containing the Game state with a passkey
-- ph for typesafe board indexing.
type GameM ph m a =
    forall board.
    ( MBoard board m
    , State.MonadIO m -- we just use this for logging
    , MonadFail m
    , Item board ~ Block
    ) =>
    StateT (Game (board ph) ph) m a

playerState :: GameM ph m (PlayerState ph)
playerState = State.gets $ \(Game (_, s) _ _) -> s

playerPos :: GameM ph m (Index ph)
playerPos = State.gets $ \(Game (p, _) _ _) -> p

canBreathe :: Block -> Bool
canBreathe blockType = blockType == Air || blockType == Stairs

canBreatheAt i = canBreathe <$> blockTypeAt i

blockTypeAt ::
    Index ph ->
    GameM ph m Block
blockTypeAt i = State.get >>= (\(Game _ board _) -> lift $ board ! i)

neighbours :: Index ph -> GameM ph m [Index ph]
neighbours (unIndex -> i) =
    fmap catMaybes
        . mapM justify'
        $ [Coord x' y' | x' <- [x i - 1 .. x i + 1], y' <- [y i - 1 .. y i + 1]]

-- Allow for uniform treatment of Indices and AdjacentPairs
-- A lawful instance gives a invertible semigroup action of Dir.
class Spatial a ph where
    (.>) :: a -> Dir -> GameM ph m (Maybe a)

instance Spatial (Index ph) ph where
    i .> dir = justify' (movePos' (unIndex i) dir)
      where
        movePos' :: Coord -> Dir -> Coord
        movePos' c GoLeft = Coord (x c - 1) (y c)
        movePos' c GoRight = Coord (x c + 1) (y c)
        movePos' c GoUp = Coord (x c) (y c - 1)
        movePos' c GoDown = Coord (x c) (y c + 1)

instance Spatial (AdjacentPair ph) ph where
    (i, j) .> dir =
        runMaybeT $ do
            i' <- MaybeT $ i .> dir
            j' <- MaybeT $ j .> dir
            pure (i', j')

mi ^> dir = mi >>= \i -> i .> dir

class BlockType a where
    (.~) :: Index ph -> a -> GameM ph m Bool

i !~ t = fmap not $ i .~ t

instance BlockType Block where
    i .~ t = t ==^ blockTypeAt i

data Ground = Ground
data Heavy = Heavy

instance BlockType Ground where
    i .~ Ground = do
        t <- blockTypeAt i
        return $ t /= Air && t /= Stairs

instance BlockType Heavy where
    i .~ Heavy = do
        t <- blockTypeAt i
        return $ t == Stone || t == Fire

(^~) :: (BlockType a) => GameM ph m (Index ph) -> a -> GameM ph m Bool
mi ^~ t = mi >>= \i -> i .~ t

(^&&^) :: (Monad m) => m Bool -> m Bool -> m Bool
(^&&^) = liftM2 (&&)

(==^) :: (Monad m, Eq a) => a -> m a -> m Bool
a ==^ ma = (a ==) <$> ma

justify' ::
    Coord ->
    GameM ph m (Maybe (Index ph))
justify' i =
    State.get >>= \(Game _ board _) -> lift $ justify board i
