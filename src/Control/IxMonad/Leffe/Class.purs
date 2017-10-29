module Control.IxMonad.Leffe.Class ( class IxMonadLeffe
                                   , getResource
                                   , addResource
                                   , modifyResource
                                   , removeResource
                                   , replaceResource
                                   , class IxMonadLeffeTrans
                                   , ilift
                                   ) where

import Prelude

import Control.IxMonad (class IxMonad)
import Data.Symbol (class IsSymbol, SProxy)
import Type.Row (class RowLacks)

class IxMonad m <= IxMonadLeffe m where

  -- Retrieves a labeled effect's resource.
  getResource
    :: forall label resource r r'
     . IsSymbol label
    => RowCons label resource r r'
    => SProxy label
    -> m (Record r') (Record r') resource

  -- Adds a new labeled effect.
  addResource
    :: forall label resource r r'
     . IsSymbol label
    => RowLacks label r
    => RowCons label resource r r'
    => SProxy label
    -> resource
    -> m (Record r) (Record r') Unit

  -- Modifies an existing labeled effect.
  modifyResource
    :: forall label r ri ro a b
     . IsSymbol label
    => RowCons label a r ri
    => RowCons label b r ro
    => SProxy label
    -> (a -> b)
    -> m (Record ri) (Record ro) Unit

  -- Removes a labeled effect.
  removeResource
    :: forall label resource r r'
     . IsSymbol label
    => RowLacks label r
    => RowCons label resource r r'
    => SProxy label
    -> m (Record r') (Record r) Unit


-- Replaces an existing labeled effect.
replaceResource
  :: forall m label r ri ro a b
   . IxMonadLeffe m
  => IsSymbol label
  => RowCons label a r ri
  => RowCons label b r ro
  => SProxy label
  -> b
  -> m (Record ri) (Record ro) Unit
replaceResource l b = modifyResource l (const b)


class IxMonadLeffeTrans t where
  ilift :: forall m i a. Monad m => m a -> t m i i a
