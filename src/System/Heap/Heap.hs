{-# LANGUAGE
    FlexibleContexts
  , GeneralizedNewtypeDeriving
  #-}
module System.Heap.Heap where

import Control.Applicative
import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader
import Data.Binary
import System.Heap.Error
import System.Heap.Pointer
import System.IO
import qualified System.Heap.Alloc as Alloc
import qualified System.Heap.Read  as Read
import qualified System.Heap.Write as Write

newtype Heap a = Heap (Write.Heap a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadError HeapError
           )

write :: Binary a => a -> Heap (Pointer a)
write = Heap . Write.write

writeRoot :: Binary a => a -> Heap ()
writeRoot = Heap . Write.writeRoot

read :: Binary a => Pointer a -> Heap a
read = Heap . Write.read

readRoot :: Binary a => Heap a
readRoot = Heap Write.readRoot

run :: FilePath -> Heap a -> IO (Either HeapError a)
run f (Heap comp) =
  do me <- try $
       withBinaryFile f ReadWriteMode $ \h -> runner h $
         do initialize
            a <- comp
            Write.writeAllocationMap
            return a
     case me of
       Left  e -> return (Left (IOError (show (e :: SomeException))))
       Right r -> return r
  where runner h = Read.run h . Alloc.run . Write.run

initialize :: Write.Heap ()
initialize =
  do fs <- ask >>= liftIO . hFileSize
     if fs == 0

       -- Initialize new heap.
       then do _ <- Write.write magic     -- magic file type identifier
               _ <- Write.write nullPtr   -- pointer to allocation map
               _ <- Write.write nullPtr   -- pointer to data structure root
               return ()

       -- Initialize existing heap.
       else do m <- Write.read 0
               when (m /= magic) (throwError InvalidHeapMagic)
               Write.readAllocationMap
               return ()

