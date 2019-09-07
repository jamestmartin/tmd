module Control.Monad.Indexed where

class IndexedMonad m where
  ireturn :: a -> m st st a
  ibind   :: m st st' a -> (a -> m st' st'' b) -> m st st'' b

data Program transition st st' a where
  Return :: a -> Program t st st a
  Command :: transition st st' a -> Program transition st st' a
  Bind :: Program t st st' a -> (a -> Program t st' st'' b) -> Program t st st'' b

instance IndexedMonad (Program transition) where
  ireturn = Return
  ibind   = Bind

iskip :: IndexedMonad m => m st st' () -> m st' st'' a -> m st st'' a
iskip m n = m `ibind` \() -> n

runProgram :: forall m transition st st' a. Monad m => (forall st st' a. transition st st' a -> m a) -> Program transition st st' a -> m a
runProgram execute = runProgram'
  where runProgram' :: forall st st' a. Program transition st st' a -> m a
        runProgram' (Return x)    = return x
        runProgram' (Bind x f)    = runProgram' x >>= runProgram' . f
        runProgram' (Command cmd) = execute cmd
