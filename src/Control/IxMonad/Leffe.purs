module Control.IxMonad.Leffe ( module Control.IxMonad.Leffe.Class
                             , Leffe
                             , runLeffe
                             ) where

import Prelude

import Control.IxMonad (class IxMonad, ibind, ipure)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Record as Record
import Data.Tuple (Tuple(Tuple), fst)
import Control.IxMonad.Leffe.Class ( class IxMonadLeffe, getResource, addResource, removeResource, replaceResource, modifyResource, class IxMonadLeffeTrans, ilift )


newtype Leffe m i o a = Leffe (i -> m (Tuple a o))


runLeffe :: forall m a. Functor m ⇒ Leffe m {} {} a -> m a
runLeffe (Leffe m) = map fst (m {})


instance ixMonadLeffeLeffe :: Monad m ⇒ IxMonadLeffe (Leffe m) where
  getResource label =
    Leffe $ \c -> pure (Tuple (Record.get label c) c)
  addResource label resource =
    Leffe $ \c -> pure (Tuple unit (Record.insert label resource c))
  modifyResource label f =
    Leffe $ \c -> pure (Tuple unit (Record.modify label f c))
  removeResource label =
    Leffe $ \c -> pure (Tuple unit (Record.delete label c))


instance ixMonadLeffe :: Monad m ⇒ IxMonad (Leffe m) where
  ipure x = Leffe $ \s -> pure (Tuple x s)
  ibind (Leffe ma) f =
    Leffe $ \s ->
      ma s >>= \(Tuple x s') ->
      case f x of
        Leffe a -> a s'

instance functorLeffe :: Monad m => Functor (Leffe m i i) where
  map f (Leffe a) =
    Leffe $ \s ->
      a s >>= \(Tuple a' s') ->
      pure (Tuple (f a') s')


instance applyLeffe :: Monad m => Apply (Leffe m i i) where
  apply (Leffe f) (Leffe a) =
    Leffe $ \s ->
      f s >>= \(Tuple f' s') ->
      a s' >>= \(Tuple a' s'') ->
      pure (Tuple (f' a') s'')


instance applicativeLeffe :: Monad m => Applicative (Leffe m i i) where
  pure = ipure


instance bindLeffe :: Monad m ⇒ Bind (Leffe m i i) where
  bind = ibind


instance monadLeffe :: (Monad m, Applicative m) => Monad (Leffe m i i)


instance monadEffLeffe :: MonadEff e m ⇒ MonadEff e (Leffe m i i) where
  liftEff e = Leffe $ \s -> do
    x <- liftEff e
    pure (Tuple x s)


instance ixMonadLeffeTransLeffe :: IxMonadLeffeTrans Leffe where
  ilift a = Leffe $ \s -> do
    x <- a
    pure (Tuple x s)
