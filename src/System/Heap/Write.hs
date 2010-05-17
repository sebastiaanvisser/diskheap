{-# LANGUAGE
    MultiParamTypeClasses
  , DeriveFunctor
  , GeneralizedNewtypeDeriving
  #-}
module System.Heap.Write where

import Prelude hiding (read)
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.Binary (Binary, encode)
import System.Heap.Error
import System.Heap.Pointer
import System.IO
import qualified Data.ByteString.Lazy as Lazy
import qualified System.Heap.Read  as Read
import qualified System.Heap.Alloc as Alloc

newtype Heap a = Heap { run :: Alloc.Heap a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState  Alloc.Map
    , MonadReader Handle
    , MonadError  HeapError
    )

runAlloc :: Alloc.Heap a -> Heap a
runAlloc = Heap

runRead :: Read.Heap a -> Heap a
runRead = runAlloc . Alloc.read

allocate :: Size -> Heap (Offset, Size)
allocate = runAlloc . Alloc.allocate

read :: Binary a => Pointer a -> Heap a
read = runRead . Read.read

writeBlock :: Offset -> Size -> Lazy.ByteString -> Heap ()
writeBlock o s bs =
  do h <- ask
     liftIO $
       do hSeek h AbsoluteSeek (fromIntegral o)
          Lazy.hPut h (encode (s, bs))

write :: Binary a => a -> Heap (Pointer a)
write a =
  do let bs = encode a
     (o, s) <- allocate (fromIntegral (Lazy.length bs))
     writeBlock o s bs
     return (Ptr o)

writeAllocationMap :: Heap ()
writeAllocationMap = get >>= write >>= writeBlock magicSize pointerSize . encode

readAllocationMap :: Heap ()
readAllocationMap = read nullPtr >>= read >>= put

