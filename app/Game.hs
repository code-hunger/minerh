{-# LANGUAGE NoFieldSelectors #-}

module Game (Block (..), Game (..), Dir (..), PlayerState (..), runPlayerUp, movePlayer) where

import Board (Index)
import Control.Monad.Extra
import qualified Control.Monad.State.Strict as State
import Game.Core
import Game.Update

movePlayer :: Dir -> GameM ph m ()
movePlayer dir = do
    (Game (playerPos, playerState) _ _) <- State.get
    let alreadyWantsToDigThere =
            -- if there's alread a dig request registered in that direction,
            -- we want to ignore it
            case playerState of
                (Digging requestDir _ _) -> requestDir == dir
                _ -> False

    unlessM isFalling $
        whenJustM (playerPos `move` dir) $
            unless alreadyWantsToDigThere
                . doMove
  where
    doMove :: AdjacentPair ph -> GameM ph m ()
    doMove (moveFrom, moveTo) = do
        nextBlockType <- blockTypeAt moveTo
        let needsToDig = nextBlockType == Dirt
        when needsToDig $
            State.modify' (\g -> g{player = (moveFrom, Digging dir 4 moveTo)})
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
            fallingState' <- computeNewFallState moveTo
            case (fallingState', dir) of
                (Falling, GoDown) ->
                    State.modify' (\g -> g{player = (moveFrom, fallingState')})
                _ ->
                    State.modify' (\g -> g{player = (moveTo, fallingState')})

move ::
    Index ph ->
    Dir ->
    GameM ph m (Maybe (Index ph, Index ph))
i `move` dir =
    i .> dir >>= \case
        Nothing -> pure Nothing
        Just j -> pure $ Just (i, j)
runPlayerUp :: Dir -> GameM ph m ()
runPlayerUp dir = do
    p <- playerPos
    whenM isStanding $
        whenJustM (p .> dir) $ \nextPos ->
            State.modify' $
                \g -> g{player = (p, Running dir nextPos)}

isFalling :: GameM ph m Bool
isFalling = (\case Falling -> True; _ -> False) <$> playerState

isStanding :: GameM ph m Bool
isStanding = (\case Standing -> True; _ -> False) <$> playerState
