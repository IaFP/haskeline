{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators, UndecidableSuperClasses #-}
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
#if MIN_VERSION_base(4,16,0)
import GHC.Types (type(@), Total)
#endif

import Data.IORef

class (
#if MIN_VERSION_base(4,16,0)
    m @ r, 
#endif
  Monad m) => MonadReader r m where
    ask :: m r

instance (
#if MIN_VERSION_base(4,16,0)
  m @ r, 
#endif
  Monad m) => MonadReader r (ReaderT r m) where
    ask = Reader.ask

instance (
#if MIN_VERSION_base(4,16,0)
  m @ s, m @ (s, s), m @ (s, ()), m @ ((), s),
#endif
  Monad m) => MonadReader s (StateT s m) where
    ask = get

instance {-# OVERLAPPABLE #-} (
#if MIN_VERSION_base(4,16,0)
  Total m, t m @ r, 
#endif
  MonadReader r m, MonadTrans t, Monad (t m))
    => MonadReader r (t m) where
    ask = lift ask

asks :: (MonadReader r m) => (r -> a) -> m a
asks f = liftM f ask

class (
#if MIN_VERSION_base(4,16,0)
  m @ s, m @ (),
#endif
  Monad m) => MonadState s m where
    get :: m s
    put :: s -> m ()

gets :: MonadState s m => (s -> a) -> m a
gets f = liftM f get

modify :: (MonadState s m) => (s -> s) -> m ()
modify f = get >>= put . f

update :: (
#if MIN_VERSION_base(4,16,0)
  m @ (s,s), m @ (s, ()),
#endif
  MonadState s m) => (s -> (a,s)) -> m a
update f = do
    s <- get
    let (x,s') = f s
    put s'
    return x

runReaderT' :: r -> ReaderT r m a -> m a
runReaderT' = flip runReaderT

instance (
#if MIN_VERSION_base(4,16,0)
  m @ (s, s), m @ (s, ()), m @ ((), s),
#endif
  Monad m) => MonadState s (StateT s m) where
    get = State.get
    put x = State.put $! x

instance {-# OVERLAPPABLE #-} (
#if MIN_VERSION_base(4,16,0)
  Total m, t m @ s, t m @ (),
#endif
  MonadState s m, MonadTrans t, Monad (t m))
    => MonadState s (t m) where
    get = lift get
    put = lift . put

-- ReaderT (IORef s) is better than StateT s for some applications,
-- since StateT loses its state after an exception such as ctrl-c.
instance (MonadIO m) => MonadState s (ReaderT (IORef s) m) where
    get = ask >>= liftIO . readIORef
    put s = ask >>= liftIO . flip writeIORef s

evalStateT' :: Monad m => s -> StateT s m a -> m a
evalStateT' s f = liftM fst $ runStateT f s

orElse :: Monad m => MaybeT m a -> m a -> m a
orElse (MaybeT f) g = f >>= maybe g return
