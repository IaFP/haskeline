{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators, UndecidableSuperClasses #-}
#endif

module System.Console.Haskeline.Monads(
                MonadTrans(..),
                MonadIO(..),
                ReaderT,
                runReaderT,
                runReaderT',
                mapReaderT,
                asks,
                StateT,
                runStateT,
                evalStateT',
                mapStateT,
                gets,
                modify,
                update,
                MonadReader(..),
                MonadState(..),
                MaybeT(MaybeT),
                runMaybeT,
                orElse
                ) where

import Control.Monad (liftM)
import Control.Monad.Catch ()
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT),runMaybeT)
import Control.Monad.Trans.Reader hiding (ask,asks)
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.State.Strict hiding (get, put, gets, modify)
import qualified Control.Monad.Trans.State.Strict as State

import Data.IORef
#if MIN_VERSION_base(4,14,0)
import GHC.Types (type (@@), Total)
#endif

class Monad m => MonadReader r m where
    ask :: m r

instance (Monad m
#if MIN_VERSION_base(4,14,0)
         , Total m
#endif
         ) => MonadReader r (ReaderT r m) where
    ask = Reader.ask

instance (Monad m
#if MIN_VERSION_base(4,14,0)
         , Total m
#endif
         ) => MonadReader s (StateT s m) where
    ask = get

instance {-# OVERLAPPABLE #-} (MonadReader r m, MonadTrans t, Monad (t m)
#if MIN_VERSION_base(4,14,0)
                              , m @@ r, t @@ m
#endif
                              ) => MonadReader r (t m) where
    ask = lift ask

asks :: (MonadReader r m
#if MIN_VERSION_base(4,14,0)
         , m @@ r
#endif
        ) => (r -> a) -> m a
asks f = liftM f ask

class Monad m => MonadState s m where
    get :: m s
    put :: s -> m ()

gets :: (MonadState s m
#if MIN_VERSION_base(4,14,0)
         , m @@ s
#endif
        ) => (s -> a) -> m a
gets f = liftM f get

modify :: (MonadState s m
#if MIN_VERSION_base(4,14,0)
         , m @@ s
#endif
          ) => (s -> s) -> m ()
modify f = get >>= put . f

update :: (MonadState s m
#if MIN_VERSION_base(4,14,0)
         , m @@ s, m @@ ()
#endif
          ) => (s -> (a,s)) -> m a
update f = do
    s <- get
    let (x,s') = f s
    put s'
    return x

runReaderT' :: r -> ReaderT r m a -> m a
runReaderT' = flip runReaderT

instance (Monad m
#if MIN_VERSION_base(4,14,0)
         , Total m
#endif
         ) => MonadState s (StateT s m) where
    get = State.get
    put x = State.put $! x

instance {-# OVERLAPPABLE #-} (MonadState s m, MonadTrans t, Monad (t m)
#if MIN_VERSION_base(4,14,0)
                              , m @@ s, t @@ m, m @@ ()
#endif
                              ) => MonadState s (t m) where
    get = lift get
    put = lift . put

-- ReaderT (IORef s) is better than StateT s for some applications,
-- since StateT loses its state after an exception such as ctrl-c.
instance (MonadIO m
#if MIN_VERSION_base(4,14,0)
         , Total m, Total (ReaderT (IORef s) m)
#endif
         ) => MonadState s (ReaderT (IORef s) m) where
    get = ask >>= liftIO . readIORef
    put s = ask >>= liftIO . flip writeIORef s

evalStateT' :: (Monad m) => s -> StateT s m a -> m a
evalStateT' s f = liftM fst $ runStateT f s

orElse :: Monad m => MaybeT m a -> m a -> m a
orElse (MaybeT f) g = f >>= maybe g return
