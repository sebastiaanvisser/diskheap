{-# LANGUAGE
    TemplateHaskell
  , TypeOperators
  , MultiParamTypeClasses
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  #-}
module System.Heap.Alloc where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Data.Binary
import Data.IntMap (IntMap)
import Data.Record.Label
import System.Heap.Error
import System.Heap.Pointer
import System.IO
import qualified Data.IntMap as Im
import qualified System.Heap.Read as Read
import qualified Control.Monad.State as State

data Map =
  Map
  { _size   :: Size
  , _unused :: IntMap [Offset]
  } deriving Show

instance Binary Map where
  put (Map s u) = put s >> put u
  get = Map <$> get <*> get

$(mkLabels [''Map])

newtype Heap a = Heap { unHeap :: State.StateT Map Read.Heap a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , State.MonadState  Map
    , MonadReader       Handle
    , MonadError        HeapError
    )

emptyAllocationMap :: Map
emptyAllocationMap = Map fileHeaderSize Im.empty

run :: Heap a -> Read.Heap a
run c = State.evalStateT (unHeap d) emptyAllocationMap
  where d = dumpMap 0 >> allocate pointerSize >> dumpMap 1 >> allocate pointerSize >> dumpMap 2 >> c

read :: Read.Heap a -> Heap a
read = Heap . lift

dumpMap :: Int -> Heap ()
dumpMap a = State.get >>= liftIO . print . (,) a

allocate :: Size -> Heap (Offset, Size)
allocate s =
  do b <- findFreeBlock s
     case b of
       Nothing     -> flip (,) s <$> getM size <* modM size (+(8+s))
       Just (t, o) -> return (o, fromIntegral t)

findFreeBlock :: Size -> Heap (Maybe (Int, Offset))
findFreeBlock s = find <$> getM unused
  where find = fmap (fmap head . fst)
             . Im.minViewWithKey
             . snd
             . Im.split (fromIntegral s - 1)

