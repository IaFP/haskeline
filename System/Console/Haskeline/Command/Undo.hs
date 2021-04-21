{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors #-}
#endif
module System.Console.Haskeline.Command.Undo where

import System.Console.Haskeline.Command
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads

import Control.Monad
#if MIN_VERSION_base(4,14,0)
import GHC.Types (Total)
#endif

data Undo = Undo {pastUndo, futureRedo :: [InsertMode]}

type UndoT = StateT Undo

runUndoT :: Monad m => UndoT m a -> m a
runUndoT = evalStateT' initialUndo

initialUndo :: Undo
initialUndo = Undo {pastUndo = [emptyIM], futureRedo = []}


saveToUndo :: Save s => s -> Undo -> Undo
saveToUndo s undo
    | not isSame = Undo {pastUndo = toSave:pastUndo undo,futureRedo=[]}
    | otherwise = undo
  where
    toSave = save s
    isSame = case pastUndo undo of
                u:_ | u == toSave -> True
                _ -> False

undoPast, redoFuture :: Save s => s -> Undo -> (s,Undo)
undoPast ls u@Undo {pastUndo = []} = (ls,u)
undoPast ls u@Undo {pastUndo = (pastLS:lss)}
        = (restore pastLS, u {pastUndo = lss, futureRedo = save ls : futureRedo u})

redoFuture ls u@Undo {futureRedo = []} = (ls,u)
redoFuture ls u@Undo {futureRedo = (futureLS:lss)}
            = (restore futureLS, u {futureRedo = lss, pastUndo = save ls : pastUndo u})



saveForUndo :: (Save s, MonadState Undo m
#if MIN_VERSION_base(4,14,0)
               , Total m
#endif
               )
                => Command m s s
saveForUndo s = do
    modify (saveToUndo s)
    return s

commandUndo, commandRedo :: (MonadState Undo m, Save s
#if MIN_VERSION_base(4,14,0)
                            , Total m
#endif
                            ) => Command m s s
commandUndo = simpleCommand $ liftM Right . update . undoPast
commandRedo = simpleCommand $ liftM Right . update . redoFuture

