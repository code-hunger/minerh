{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoFieldSelectors #-}

module Game (Block (..), Game (..), Dir (..), PlayerState (..), runPlayerUp, movePlayer) where

import Board (Index)
import Control.Monad.Extra
import Control.Monad.RWS.Strict (MonadTrans (lift))
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Functor ((<&>))
import Game.Core
import Game.Update

movePlayer :: Dir -> GameM board ph m ()
movePlayer dir =
    let alreadyWantsToDigThere =
            -- if there's alread a dig request registered in that direction,
            -- we want to ignore it
            playerState <&> \case
                (Digging requestDir _ _) -> requestDir == dir
                _ -> False
     in withMaybe $ do
            guardM $ not <$> alreadyWantsToDigThere
            guardM $ not <$> isFalling
            target <- MaybeT (playerPosM ^> dir)
            lift $ doMove target
  where
    doMove :: Index ph -> GameM board ph m ()
    doMove moveTo = do
        moveFrom <- playerPosM
        nextBlockType <- blockTypeAt moveTo
        let needsToDig = nextBlockType == Dirt
        when needsToDig $
            State.modify' (\g -> g{player = Digging dir 4 (moveFrom, moveTo)})
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
            State.modify' (\g -> g{player = fallingState'})

runPlayerUp :: Dir -> GameM board ph m ()
runPlayerUp dir = do
    p <- playerPosM
    whenM isStanding $
        whenJustM (p .> dir) $ \nextPos ->
            State.modify' $
                \g -> g{player = Running dir (p, nextPos)}

isFalling :: GameM board ph m Bool
isFalling = (\case Falling _ -> True; _ -> False) <$> playerState

isStanding :: GameM board ph m Bool
isStanding = (\case Standing _ -> True; _ -> False) <$> playerState
