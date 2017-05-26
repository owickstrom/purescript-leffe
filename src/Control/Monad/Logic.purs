module Control.Monad.Logic where

import Prelude
import Control.Monad.Logic.Record as Record
import Control.Alt (class Alt)
import Control.IxMonad (class IxMonad, ibind, ipure)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Identity (Identity)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Tuple (Tuple(..), fst)

data LVar a = Unbound | Bound a

class (IxMonad m) <= IxMonadLogic m where
  fresh
    ∷ ∀ name a r r'
     . RowCons name (LVar a) r r'
     => IsSymbol name
     => SProxy name
     -> m (Record r) (Record r') (LVar a)

newtype Logic m i o a = Logic (i -> m (Tuple a o))

runLogic :: forall m i o a. Functor m => Logic m i o a -> i -> m (Tuple a o)
runLogic (Logic m) x = m x

execLogic :: forall m i o a. Functor m => Logic m i o a -> i -> m a
execLogic l x = fst <$> runLogic l x

hoistLogic :: forall f g i o a. (f ~> g) -> Logic f i o a -> Logic g i o a
hoistLogic f (Logic k) = Logic (f <<< k)

instance ixMonadLogicLogic :: (Monad m, Applicative m) ⇒ IxMonadLogic (Logic m) where
  fresh name =
    let lvar = Unbound
    in Logic (\r -> pure (Tuple lvar (Record.insert name lvar r)))

instance ixMonadLogic :: Monad m ⇒ IxMonad (Logic m) where
  ipure x = Logic $ \s -> pure (Tuple x s)
  ibind (Logic ma) f =
    Logic $ \s ->
      ma s >>= \(Tuple x s') ->
      case f x of
        Logic a -> a s'

instance functorLogic :: Monad m => Functor (Logic m i i) where
  map f a =
    Logic $ \s ->
      runLogic a s >>= \(Tuple a' s') ->
      pure (Tuple (f a') s')


instance applyLogic :: Monad m => Apply (Logic m i i) where
  apply f a =
    Logic $ \s ->
      runLogic f s >>= \(Tuple f' s') ->
      runLogic a s' >>= \(Tuple a' s'') ->
      pure (Tuple (f' a') s'')

instance applicativeLogic :: Monad m => Applicative (Logic m i i) where
  pure = ipure

instance bindLogic :: Monad m ⇒ Bind (Logic m i i) where
  bind = ibind

instance monadLogic :: (Monad m, Applicative m) => Monad (Logic m i i)

instance monadEffLogic :: MonadEff e m ⇒ MonadEff e (Logic m i i) where
  liftEff e = Logic $ \s -> do
    x <- liftEff e
    pure (Tuple x s)

instance monadAffLogic :: MonadAff e m ⇒ MonadAff e (Logic m i i) where
  liftAff e = Logic $ \s -> do
    x <- liftAff e
    pure (Tuple x s)

lift' :: forall m i a. Monad m ⇒ m a -> Logic m i i a
lift' a = Logic $ \s -> do
  x <- a
  pure (Tuple x s)



pitch :: forall m. IxMonadLogic m => m {} {} Int
pitch = do
  ipure 1
  ipure 2
  where
    bind = ibind
    discard = ibind

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = log (show (execLogic pitch {} :: Identity Int))
