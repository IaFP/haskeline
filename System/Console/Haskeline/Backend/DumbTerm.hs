#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators #-}
#endif
module System.Console.Haskeline.Backend.DumbTerm where

import System.Console.Haskeline.Backend.Posix
import System.Console.Haskeline.Backend.WCWidth
import System.Console.Haskeline.Term
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads as Monads

import System.IO
import Control.Applicative(Applicative)
import Control.Monad(liftM)
import Control.Monad.Catch
#if MIN_VERSION_base(4,14,0)
import GHC.Types (Total)
import qualified Control.Monad.Trans.State.Strict as SS
#endif

-- TODO: 
---- Put "<" and ">" at end of term if scrolls off.
---- Have a margin at the ends

data Window = Window {pos :: Int -- ^ # of visible chars to left of cursor
                }

initWindow :: Window
initWindow = Window {pos=0}
{-
-- #if MIN_VERSION_base(4,14,0)
data DumbTerm m a = DumbTerm {unDumbTerm :: StateT Window (PosixT m) a}
                -- deriving (Functor
                         -- , Applicative
                         -- , Monad
                         -- , MonadIO,
                         -- , MonadThrow, MonadCatch
                         -- , MonadMask,
                          -- MonadState Window, MonadReader Handles)
instance Total (DumbTerm m)

instance (Total m, Functor m) => Functor (DumbTerm m) where
  fmap f (DumbTerm x) = DumbTerm $ fmap f x

instance (Total m, Monad m) => Applicative (DumbTerm m) where
   pure a = DumbTerm (SS.StateT $ \s -> return (a, s))
   (DumbTerm f) <*> (DumbTerm a) = DumbTerm (f <*> a)

instance (Total m, Monad m) => Monad (DumbTerm m) where
   x >>= f = DumbTerm $ do x' <- unDumbTerm x
                           x'' <- unDumbTerm (f x')
                           return x''

instance (Total m, MonadIO m) => MonadIO (DumbTerm m) where
  liftIO = DumbTerm . liftIO
  

instance (Total m, MonadThrow m) => MonadThrow (DumbTerm m) where
  throwM = DumbTerm . throwM
  
instance (Total m, MonadCatch m) => MonadCatch (DumbTerm m) where
  catch m f = DumbTerm $ catch (unDumbTerm m) (\e -> unDumbTerm (f e))

instance (Total m, MonadMask m) => MonadMask (DumbTerm m) where
  mask a = DumbTerm $ mask $ \u -> unDumbTerm (a $ q u)
    where q :: (StateT Window (PosixT m) a -> StateT Window (PosixT m) a)
            -> DumbTerm m a -> DumbTerm m a
          q u (DumbTerm b) = DumbTerm $ u b
  uninterruptibleMask a = DumbTerm $ uninterruptibleMask $ \u -> unDumbTerm (a $ q u)
    where q :: (StateT Window (PosixT m) a -> StateT Window (PosixT m) a)
            -> DumbTerm m a -> DumbTerm m a
          q u (DumbTerm b) = DumbTerm $ u b
  generalBracket acquire release use = mask $ \unmasked -> do
    resource <- acquire
    b <- unmasked (use resource) `catch` \e ->
      do _ <- release resource (ExitCaseException e)
         throwM e
    c <- release resource (ExitCaseSuccess b)
    return (b, c)

instance (Total m, Monad m) => MonadState Window (DumbTerm m) where
  get   = DumbTerm $ SS.state (\s -> (s, s))
  put s = DumbTerm $ SS.state (\_ -> ((), s))

instance (Total m, Monad m) => MonadReader Handles (DumbTerm m) where
  ask = DumbTerm $ ask

-- #else
-}
newtype DumbTerm m a = DumbTerm {unDumbTerm :: StateT Window (PosixT m) a}
                deriving (Functor, Applicative, Monad, MonadIO,
                          MonadThrow, MonadCatch, MonadMask,
                          MonadState Window, MonadReader Handles)
-- #endif

instance Total (DumbTerm m)

type DumbTermM a = forall m . (MonadIO m, MonadReader Layout m
#if MIN_VERSION_base(4,14,0)
                              , Total m
#endif
                              ) => DumbTerm m a

instance MonadTrans DumbTerm where
    lift = DumbTerm . lift . lift

evalDumb :: (MonadReader Layout m, CommandMonad m
#if MIN_VERSION_base(4,14,0)
            , Total m
#endif
            ) => EvalTerm (PosixT m)
evalDumb = EvalTerm (evalStateT' initWindow . unDumbTerm) (DumbTerm . lift)

runDumbTerm :: Handles -> MaybeT IO RunTerm
runDumbTerm h = liftIO $ posixRunTerm h (posixLayouts h) [] id evalDumb
                                
instance (MonadIO m, MonadMask m, MonadReader Layout m
#if MIN_VERSION_base(4,14,0)
         , Total m
#endif
         ) => Term (DumbTerm m) where
    reposition _ s = refitLine s
    drawLineDiff = drawLineDiff'
    
    printLines = mapM_ (printText . (++ crlf))
    moveToNextLine _ = printText crlf
    clearLayout = clearLayoutD
    ringBell True = printText "\a"
    ringBell False = return ()
      
printText :: (MonadIO m
#if MIN_VERSION_base(4,14,0)
             , Total m
#endif
             ) => String -> DumbTerm m ()
printText str = do
    h <- liftM ehOut ask
    liftIO $ hPutStr h str
    liftIO $ hFlush h

-- Things we can assume a dumb terminal knows how to do
cr,crlf :: String
crlf = "\r\n"
cr = "\r"

backs,spaces :: Int -> String
backs n = replicate n '\b'
spaces n = replicate n ' '


clearLayoutD :: DumbTermM ()
clearLayoutD = do
    w <- maxWidth
    printText (cr ++ spaces w ++ cr)

-- Don't want to print in the last column, as that may wrap to the next line.
maxWidth :: DumbTermM Int
maxWidth = asks (\lay -> width lay - 1)

drawLineDiff' :: LineChars -> LineChars -> DumbTermM ()
drawLineDiff' (xs1,ys1) (xs2,ys2) = do
    Window {pos=p} <- get
    w <- maxWidth
    let (xs1',xs2') = matchInit xs1 xs2
    let (xw1, xw2) = (gsWidth xs1', gsWidth xs2')
    let newP = p + xw2 - xw1
    let (ys2', yw2) = takeWidth (w-newP) ys2
    if xw1 > p  || newP >= w
        then refitLine (xs2,ys2)
        else do -- we haven't moved outside the margins
            put Window {pos=newP}
            case (xs1',xs2') of
                ([],[]) | ys1 == ys2    -> return () -- no change
                (_,[]) | xs1' ++ ys1 == ys2 -> -- moved left
                    printText $ backs xw1
                ([],_) | ys1 == xs2' ++ ys2 -> -- moved right
                    printText (graphemesToString xs2')
                _ -> let extraLength = xw1 + snd (takeWidth (w-p) ys1)
                                        - xw2 - yw2
                     in printText $ backs xw1
                        ++ graphemesToString (xs2' ++ ys2') ++ clearDeadText extraLength
                        ++ backs yw2

refitLine :: ([Grapheme],[Grapheme]) -> DumbTermM ()
refitLine (xs,ys) = do
    w <- maxWidth
    let (xs',p) = dropFrames w xs
    put Window {pos=p}
    let (ys',k) = takeWidth (w - p) ys
    printText $ cr ++ graphemesToString (xs' ++ ys')
        ++ spaces (w-k-p)
        ++ backs (w-p)
  where
    -- returns the width of the returned characters.
    dropFrames w zs = case splitAtWidth w zs of
                        (_,[],l) -> (zs,l)
                        (_,zs',_) -> dropFrames w zs'

    
clearDeadText :: Int -> String
clearDeadText n | n > 0 = spaces n ++ backs n
                | otherwise = ""
