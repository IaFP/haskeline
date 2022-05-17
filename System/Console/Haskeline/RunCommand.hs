{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators, RankNTypes #-}
#endif
{-# LANGUAGE RankNTypes, FlexibleInstances
             , TypeSynonymInstances
             , FlexibleContexts, ExistentialQuantification
             , ScopedTypeVariables, GeneralizedNewtypeDeriving
             , StandaloneDeriving
             , MultiParamTypeClasses
             , UndecidableInstances
             , ScopedTypeVariables, CPP, DeriveDataTypeable
             , PatternGuards
  #-}
{-# LANGUAGE  QuantifiedConstraints, DatatypeContexts
           , DefaultSignatures, ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass, DataKinds, TypeApplications, UnliftedNewtypes, TypeFamilies, TypeOperators, PolyKinds #-}

module System.Console.Haskeline.RunCommand (runCommandLoop) where

import System.Console.Haskeline.Command
import System.Console.Haskeline.Term
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads
import System.Console.Haskeline.Prefs
import System.Console.Haskeline.Key

import Control.Exception (SomeException)
import Control.Monad
import Control.Monad.Catch (handle, throwM)
#if MIN_VERSION_base(4,16,0)
import GHC.Types (Total, type(@))
#endif

runCommandLoop :: (CommandMonad m, MonadState Layout m, LineState s)
    => TermOps -> Prefix -> KeyCommand m s a -> s -> m a
runCommandLoop tops@TermOps{evalTerm = e} prefix cmds initState
    = -- error "not so fast smarty pants"

    case e of -- NB: Need to separate this case out from the above pattern
                -- in order to build on ghc-6.12.3
        EvalTerm eval liftE ->
           eval $ withGetEvent tops 
           $ runCommandLoop' liftE tops prefix initState cmds
    
  
runCommandLoop' :: forall m n s a . (Applicative m, Term n, CommandMonad n,
        MonadState Layout m, LineState s)
        => (forall b . m b -> n b) -> TermOps -> Prefix -> s -> KeyCommand m s a -> n Event
        -> n a
runCommandLoop' liftE tops prefix initState cmds getEvent = do
    let s = lineChars prefix initState
    drawLine s
    readMoreKeys s (fmap (liftM (\x -> (x,[])) . ($ initState)) cmds)
  where
    readMoreKeys :: Applicative m => LineChars -> KeyMap (CmdM m (a,[Key])) -> n a
    readMoreKeys s next = do
        event <- handle (\(e::SomeException) -> moveToNextLine s >> throwM e)
                    getEvent
        case event of
                    ErrorEvent e -> moveToNextLine s >> throwM e
                    WindowResize -> do
                        drawReposition liftE tops s
                        readMoreKeys s next
                    KeyInput ks -> do
                        bound_ks <- mapM (asks . lookupKeyBinding) ks
                        loopCmd s $ applyKeysToMap (concat bound_ks) next
                    ExternalPrint str -> do
                        printPreservingLineChars s str
                        readMoreKeys s next
    loopCmd :: Applicative m => LineChars -> CmdM m (a,[Key]) -> n a

    loopCmd s (GetKey next) = readMoreKeys s next
    -- If there are multiple consecutive LineChanges, only render the diff
    -- to the last one, and skip the rest. This greatly improves speed when
    -- a large amount of text is pasted in at once.
    loopCmd s (DoEffect (LineChange _)
                e@(DoEffect (LineChange _) _)) = loopCmd s e
    loopCmd s (DoEffect e next) = do
                                    t <- drawEffect prefix s e
                                    loopCmd t next
    loopCmd s (CmdM next) = liftE next >>= loopCmd s
    loopCmd s (Result (x,ks)) = do
                                    liftIO (saveUnusedKeys tops ks)
                                    moveToNextLine s
                                    return x

printPreservingLineChars :: Term m => LineChars -> String -> m ()
printPreservingLineChars s str =  do
    clearLine s
    printLines . lines $ str
    drawLine s

drawReposition :: (Applicative m, Applicative n, Term n, MonadState Layout m)
    => (forall a . m a -> n a) -> TermOps -> LineChars -> n ()
drawReposition liftE tops s = do
    oldLayout <- liftE get
    newLayout <- liftIO (getLayout tops)
    liftE (put newLayout)
    when (oldLayout /= newLayout) $ reposition oldLayout s

drawEffect :: (Applicative m, Term m, MonadReader Prefs m)
    => Prefix -> LineChars -> Effect -> m LineChars
drawEffect prefix s (LineChange ch) = do
    let t = ch prefix
    drawLineDiff s t
    return t
drawEffect _ s ClearScreen = do
    clearLayout
    drawLine s
    return s
drawEffect _ s (PrintLines ls) = do
    when (s /= ([],[])) $ moveToNextLine s
    printLines ls
    drawLine s
    return s
drawEffect _ s RingBell = actBell >> return s

actBell :: (Term m, MonadReader Prefs m) => m ()
actBell = do
    style <- asks bellStyle
    case style of
        NoBell -> return ()
        VisualBell -> ringBell False
        AudibleBell -> ringBell True


---------------
-- Traverse through the tree of keybindings, using the given keys.
-- Remove as many GetKeys as possible.
-- Returns any unused keys (so that they can be applied at the next getInputLine).
applyKeysToMap :: (Monad m) => [Key] -> KeyMap (CmdM m (a,[Key]))
                                -> CmdM m (a,[Key])
applyKeysToMap [] next = GetKey next
applyKeysToMap (k:ks) next = case lookupKM next k of
    Nothing -> DoEffect RingBell $ GetKey next
    Just (Consumed cmd) -> applyKeysToCmd ks cmd
    Just (NotConsumed cmd) -> applyKeysToCmd (k:ks) cmd

applyKeysToCmd :: (Monad m) => [Key] -> CmdM m (a,[Key])
                                -> CmdM m (a,[Key])
applyKeysToCmd ks (GetKey next) = applyKeysToMap ks next
applyKeysToCmd ks (DoEffect e next) = DoEffect e (applyKeysToCmd ks next)
applyKeysToCmd ks (CmdM next) = CmdM $ liftM (applyKeysToCmd ks) next
applyKeysToCmd ks (Result (x,ys)) = Result (x,ys++ks) -- use in the next input line
