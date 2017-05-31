module Control.Monad.Leffe.File ( kind FileMode
                                , Read
                                , Write
                                , File
                                , class FileOpen
                                , stat
                                , openFile
                                , readFile
                                , writeFile
                                , class FileClose
                                , closeFile
                                ) where

import Prelude
import Control.IxMonad (ibind, (:>>=))
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Leffe (Leffe, addLeffe, getLeffe, lift', removeLeffe)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy)
import Node.Buffer (BUFFER, Buffer)
import Node.FS (FileDescriptor, FileFlags(W, R)) as FS
import Node.FS (BufferOffset, BufferLength)
import Node.FS.Aff (fdOpen, fdRead, fdWrite, fdClose, stat) as FS
import Node.FS.Aff (FS)
import Node.FS.Stats (Stats)
import Node.Path (FilePath)

foreign import kind FileMode

foreign import data Read :: FileMode
foreign import data Write :: FileMode

data File (mode :: FileMode) = File FilePath FS.FileDescriptor

class FileOpen m (mode :: FileMode) where
  openFile
    :: forall l r r'
    . IsSymbol l
    => RowCons l (File mode) r r'
    => SProxy l
    -> FilePath
    -> Leffe m (Record r) (Record r') Unit

instance fileOpenRead :: MonadAff (fs :: FS | e) m => FileOpen m Read where
  openFile label path = do
    fd <- lift' $ liftAff $ FS.fdOpen path FS.R Nothing
    addLeffe label (File path fd)
    where
        bind = ibind

instance fileOpenWrite :: MonadAff (fs :: FS | e) m => FileOpen m Write where
  openFile label path = do
    fd <- lift' $ liftAff $ FS.fdOpen path FS.W Nothing
    addLeffe label (File path fd)
    where
        bind = ibind

stat
  :: forall m e l r r' mode
   . MonadAff (fs :: FS | e) m
  => IsSymbol l
  => RowCons l (File mode) r r'
  => SProxy l
  -> Leffe m (Record r') (Record r') Stats
stat label =
  getLeffe label
  :>>= \(File path _) -> lift' $ liftAff $ FS.stat path

readFile
  :: forall m e l r r'
   . MonadAff (fs :: FS, buffer :: BUFFER | e) m
  => IsSymbol l
  => RowCons l (File Read) r r'
  => SProxy l
  -> Buffer
  -> BufferOffset
  -> BufferLength
  -> Leffe m (Record r') (Record r') Int
readFile label buf offset length = do
  File path fd <- getLeffe label
  lift' $ liftAff $ FS.fdRead fd buf offset length Nothing
  where
    bind = ibind
    discard = ibind

writeFile
  :: forall m e l r r'
   . MonadAff (fs :: FS, buffer :: BUFFER | e) m
  => IsSymbol l
  => RowCons l (File Write) r r'
  => SProxy l
  -> Buffer
  -> BufferOffset
  -> BufferLength
  -> Leffe m (Record r') (Record r') Unit
writeFile label contents offset length = do
  File path fd <- getLeffe label
  lift' $ liftAff (FS.fdWrite fd contents offset length Nothing)
  pure unit
  where
    bind = ibind
    discard = ibind

class FileClose m (mode :: FileMode) where
  closeFile
    :: forall l i o
    . IsSymbol l
    => RowCons l (File mode) o i
    => SProxy l
    -> Leffe m (Record i) (Record o) Unit

closeFile'
  :: forall m e l i o mode
   . IsSymbol l
  => MonadAff (fs :: FS | e) m
  => RowCons l (File mode) o i
  => SProxy l
  -> Leffe m (Record i) (Record o) Unit
closeFile' label = do
  File _ fd <- getLeffe label
  lift' $ liftAff $ FS.fdClose fd
  removeLeffe label
  where
    bind = ibind
    discard = ibind

instance fileCloseRead :: MonadAff (fs :: FS | e) m => FileClose m Read where
  closeFile = closeFile'

instance fileCloseWrite :: MonadAff (fs :: FS | e) m => FileClose m Write where
  closeFile = closeFile'
