module Control.IxMonad.Leffe where

import Prelude

import Control.IxMonad (class IxMonad, ibind, ipure)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Record as Record
import Data.Symbol (class IsSymbol, SProxy)
import Data.Tuple (Tuple(Tuple), fst)
import Type.Row (class RowLacks)

class IxMonadLeffe m where

  -- Retrieves a labeled effect's resource.
  getLeffe
    :: forall label resource r r'
     . IsSymbol label
    => RowCons label resource r r'
    => SProxy label
    -> m {|r'} {|r'} resource

  -- Adds a new labeled effect.
  addLeffe
    :: forall label resource r r'
     . IsSymbol label
    => RowLacks label r
    => RowCons label resource r r'
    => SProxy label
    -> resource
    -> m {|r} {|r'} Unit

  -- Removes a labeled effect.
  removeLeffe
    :: forall label resource r r'
     . IsSymbol label
    => RowLacks label r
    => RowCons label resource r r'
    => SProxy label
    -> m {|r'} {|r} Unit


newtype Leffe m i o a = Leffe (i -> m (Tuple a o))


runLeffe :: forall m a. Functor m ⇒ Leffe m {} {} a -> m a
runLeffe (Leffe m) = map fst (m {})


instance ixMonadLeffeLeffe :: Applicative m ⇒ IxMonadLeffe (Leffe m) where
  getLeffe label =
    Leffe $ \c -> pure (Tuple (Record.get label c) c)
  addLeffe label resource =
    Leffe $ \c -> pure (Tuple unit (Record.insert label resource c))
  removeLeffe label =
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


lift' :: forall m i a. Monad m ⇒ m a -> Leffe m i i a
lift' a = Leffe $ \s -> do
  x <- a
  pure (Tuple x s)


instance monadEffLeffe :: MonadEff e m ⇒ MonadEff e (Leffe m i i) where
  liftEff e = Leffe $ \s -> do
    x <- liftEff e
    pure (Tuple x s)
