module Control.Monad.Leff where

import Prelude
import Control.Monad.Leff.Record as Record
import Control.IxMonad (class IxMonad, ibind, ipure)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Tuple (Tuple(Tuple), fst)

class IxMonadLeff m where

  -- Retrieves a labeled effect's resource.
  getLeff
    :: forall label resource eff eff'
     . IsSymbol label
    => RowCons label resource eff eff'
    => SProxy label
    -> m (Record eff') (Record eff') resource

  -- Adds a new labeled effect.
  addLeff
    :: forall label resource eff eff'
     . IsSymbol label
    => RowCons label resource eff eff'
    => SProxy label
    -> resource
    -> m (Record eff) (Record eff') Unit

  -- Removes a labeled effect.
  removeLeff
    :: forall label resource eff eff'
     . IsSymbol label
    => RowCons label resource eff eff'
    => SProxy label
    -> m (Record eff') (Record eff) Unit


newtype Leff m i o a = Leff (i -> m (Tuple a o))


runLeff :: forall m a. Functor m ⇒ Leff m {} {} a -> m a
runLeff (Leff m) = map fst (m {})


instance ixMonadLeffLeff :: Applicative m ⇒ IxMonadLeff (Leff m) where
  getLeff label =
    Leff $ \c -> pure (Tuple (Record.get label c) c)
  addLeff label resource =
    Leff $ \c -> pure (Tuple unit (Record.insert label resource c))
  removeLeff label =
    Leff $ \c -> pure (Tuple unit (Record.delete label c))


instance ixMonadLeff :: Monad m ⇒ IxMonad (Leff m) where
  ipure x = Leff $ \s -> pure (Tuple x s)
  ibind (Leff ma) f =
    Leff $ \s ->
      ma s >>= \(Tuple x s') ->
      case f x of
        Leff a -> a s'

instance functorLeff :: Monad m => Functor (Leff m i i) where
  map f (Leff a) =
    Leff $ \s ->
      a s >>= \(Tuple a' s') ->
      pure (Tuple (f a') s')


instance applyLeff :: Monad m => Apply (Leff m i i) where
  apply (Leff f) (Leff a) =
    Leff $ \s ->
      f s >>= \(Tuple f' s') ->
      a s' >>= \(Tuple a' s'') ->
      pure (Tuple (f' a') s'')


instance applicativeLeff :: Monad m => Applicative (Leff m i i) where
  pure = ipure


instance bindLeff :: Monad m ⇒ Bind (Leff m i i) where
  bind = ibind


instance monadLeff :: (Monad m, Applicative m) => Monad (Leff m i i)


lift' :: forall m i a. Monad m ⇒ m a -> Leff m i i a
lift' a = Leff $ \s -> do
  x <- a
  pure (Tuple x s)


instance monadEffLeff :: MonadEff e m ⇒ MonadEff e (Leff m i i) where
  liftEff e = Leff $ \s -> do
    x <- liftEff e
    pure (Tuple x s)
