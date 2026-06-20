{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Game.Core where

import Board (Coord (..), Index (unIndex), MBoard, SafeArray (Item, justify, (!)))

import Control.Monad.Extra (guard, liftM2)
import Control.Monad.State.Strict (MonadTrans (lift), StateT)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe)

data Block = Air | Dirt | Stone | Stairs | Fire
    deriving (Eq)

data Dir = GoLeft | GoRight | GoUp | GoDown
    deriving (Eq, Show, Read)

type AdjacentPair ph = (Index ph, Index ph)

type MovingPart ph =
    ( Int -- how many ticks should pass until thhe part drops by 1 block
    , Index ph -- the bottom part of a collapsing column
    , [Index ph] -- the column
    )

data PlayerState ph
    = Standing (Index ph)
    | Digging Dir Int (AdjacentPair ph)
    | Falling (AdjacentPair ph)
    | Running Dir (AdjacentPair ph)
    deriving (Show, Read)

data Game board ph = Game
    { player :: PlayerState ph
    , board :: board
    , movingParts :: [MovingPart ph]
    }

-- The GameM monad is a state monad (with IO for logging) containing the Game state with a passkey
-- ph for typesafe board indexing.
type GameM board ph m a =
    ( MBoard board m
    , State.MonadIO m -- we just use this for logging
    , Item board ~ Block
    ) =>
    StateT (Game (board ph) ph) m a

guardM x = lift x >>= guard

withMaybe :: (Functor m, Monoid a) => MaybeT m a -> m a
withMaybe = fmap (fromMaybe mempty) . runMaybeT

playerState :: GameM board ph m (PlayerState ph)
playerState = State.gets $ \(Game s _ _) -> s

setPlayerState :: PlayerState ph -> GameM board ph m ()
setPlayerState newState = State.modify $ \g -> g{player = newState}

playerPos :: PlayerState ph -> Index ph
playerPos = \case
    Standing p -> p
    Digging _ _ (p, _) -> p
    Falling (p, _) -> p
    Running _ (p, _) -> p

playerPosM :: GameM board ph m (Index ph)
playerPosM = playerPos <$> playerState

canBreathe :: Block -> Bool
canBreathe blockType = blockType == Air || blockType == Stairs

canBreatheAt i = canBreathe <$> blockTypeAt i

blockTypeAt ::
    Index ph ->
    GameM board ph m Block
blockTypeAt i = State.get >>= (\(Game _ board _) -> lift $ board ! i)

neighbours :: Index ph -> GameM board ph m [Index ph]
neighbours (unIndex -> i) =
    fmap catMaybes
        . mapM justify'
        $ [Coord x' y' | x' <- [x i - 1 .. x i + 1], y' <- [y i - 1 .. y i + 1]]

-- Allow for uniform treatment of Indices and AdjacentPairs
-- A lawful instance gives a invertible semigroup action of Dir.
class Spatial a ph where
    (.>) :: a -> Dir -> GameM board ph m (Maybe a)

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

instance Spatial (Index ph, [Index ph]) ph where
    (fallOn, column) .> dir =
        fallOn .> dir >>= \case
            Nothing -> pure Nothing
            Just belowFall ->
                (pure $ Just (belowFall, fallOn : dropLast column))
      where
        dropLast [] = error "Drop last on empty list."
        dropLast [_] = []
        dropLast (a : rest) = a : dropLast rest

mi ^> dir = mi >>= \i -> i .> dir

class BlockType a where
    (.~) :: Index ph -> a -> GameM board ph m Bool

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

(^~) :: (BlockType a) => GameM board ph m (Index ph) -> a -> GameM board ph m Bool
mi ^~ t = mi >>= \i -> i .~ t

(^&&^) :: (Monad m) => m Bool -> m Bool -> m Bool
(^&&^) = liftM2 (&&)

(==^) :: (Monad m, Eq a) => a -> m a -> m Bool
a ==^ ma = (a ==) <$> ma

justify' ::
    Coord ->
    GameM board ph m (Maybe (Index ph))
justify' i =
    State.get >>= \(Game _ board _) -> lift $ justify board i
