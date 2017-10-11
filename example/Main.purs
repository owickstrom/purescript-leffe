-- This example shows how the type system and Leffe can be used to
-- enforce closing of files, and reading/writing of files with correct
-- file modes.

module Main where

import Prelude

import Control.IxMonad (ibind, (:>>=))
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.IxMonad.Leffe (Leffe, addLeffe, getLeffe, runLeffe, lift', removeLeffe)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Node.Buffer (BUFFER, Buffer)
import Node.Buffer as Buffer
import Node.FS (BufferOffset, BufferLength)
import Node.FS (FileDescriptor, FileFlags(W, R)) as FS
import Node.FS.Aff (FS)
import Node.FS.Aff (fdOpen, fdRead, fdWrite, fdClose, stat) as FS
import Node.FS.Stats (Stats(..))
import Node.Path (FilePath)
import Type.Row (class RowLacks)

foreign import kind FileMode

foreign import data Read :: FileMode
foreign import data Write :: FileMode

data FileMode (m :: FileMode) = FileMode

modeRead :: FileMode Read
modeRead = FileMode

modeWrite :: FileMode Write
modeWrite = FileMode

data File (mode :: FileMode) = File FilePath FS.FileDescriptor

class FileOpen m (mode :: FileMode) where
  openFile
    :: forall l r r'
    . IsSymbol l
    => RowCons l (File mode) r r'
    => RowLacks l r
    => SProxy l
    -> FilePath
    -> FileMode mode
    -> Leffe m (Record r) (Record r') Unit

instance fileOpenRead :: MonadAff (fs :: FS | e) m => FileOpen m Read where
  openFile label path _ = do
    fd <- lift' $ liftAff $ FS.fdOpen path FS.R Nothing
    addLeffe label (File path fd)
    where
        bind = ibind

instance fileOpenWrite :: MonadAff (fs :: FS | e) m => FileOpen m Write where
  openFile label path _ = do
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
    => RowLacks l o
    => RowCons l (File mode) o i
    => SProxy l
    -> Leffe m (Record i) (Record o) Unit

closeFile'
  :: forall m e l i o mode
   . IsSymbol l
  => MonadAff (fs :: FS | e) m
  => RowLacks l o
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

-- Now for the usage example! We define two proxies, temp1 and temp2,
-- that we'll use to refer to our file resources.

temp1 :: SProxy "temp1"
temp1 = SProxy

temp2 :: SProxy "temp2"
temp2 = SProxy

example
  :: forall m e
   . MonadAff (fs :: FS, buffer :: BUFFER | e) m
  => Leffe m {} {} Unit
example = do
  -- Open two files, labeled temp1 and temp2, for reading and writing,
  -- respectively:
  openFile temp1 "/tmp/in.txt" modeRead
  openFile temp2 "/tmp/out.txt" modeWrite

  -- First check how large the input file is, and then create a buffer
  -- to hold it's contents. We are not doing any buffering.
  Stats stats <- stat temp1
  let size = round stats.size
  buf <- liftEff $ Buffer.create size

  -- Read the contents of one file, write to the other:
  readFile temp1 buf 0 size
  writeFile temp2 buf 0 size

  -- Close both:
  closeFile temp1
  closeFile temp2

  where
    bind = ibind
    discard = ibind

main
  :: forall e
   . Eff ( fs :: FS
         , buffer :: BUFFER
         , exception :: EXCEPTION
         | e
         ) Unit
main = void (launchAff (runLeffe example))
