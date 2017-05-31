-- This example shows how the type system and Leff can be used to
-- enforce closing of files, and reading/writing of files with correct
-- file modes.

module Main where

import Prelude
import Node.Buffer as Buffer
import Control.IxMonad (ibind)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Leff (Leff, runLeff)
import Control.Monad.Leff.File (File, Read, Write, closeFile, openFile, readFile, stat, writeFile)
import Data.Int (round)
import Data.Symbol (SProxy(SProxy))
import Node.Buffer (BUFFER)
import Node.FS.Aff (FS)
import Node.FS.Stats (Stats(..))

temp1 :: SProxy "temp1"
temp1 = SProxy

temp2 :: SProxy "temp2"
temp2 = SProxy

example
  :: forall m e
   . MonadAff (fs :: FS, buffer :: BUFFER | e) m
  => Leff m {} {} Unit
example = do
  -- Open two files, labeled temp1 and temp2, for reading and writing,
  -- respectively:
  openFile temp1 "/tmp/in.txt" :: forall r. Leff m {|r} {temp1 :: File Read|r} Unit
  openFile temp2 "/tmp/out.txt" :: forall r. Leff m {|r} {temp2 :: File Write | r} Unit

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
main = void (launchAff (runLeff example))
