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
  , _used   :: IntMap [Offset]
  , _unused :: IntMap [Offset]
  } deriving Show

instance Binary Map where
  put (Map s u v) = put s *> put u *> put v
  get = Map <$> get <*> get <*> get

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
emptyAllocationMap = Map 0 Im.empty Im.empty

run :: Heap a -> Read.Heap a
run (Heap c) = State.evalStateT c emptyAllocationMap

read :: Read.Heap a -> Heap a
read = Heap . lift

dumpMap :: Int -> Heap ()
dumpMap a = State.get >>= liftIO . print . (,) a

allocate :: Size -> Heap (Offset, Size)
allocate s =
  do b <- findFreeBlock s
     (o, t) <- case b of
       Nothing     -> flip (,) s <$> getM size <* modM size (+(16+s))
       Just (t, o) -> return (o, fromIntegral t)
     modM used (Im.alter (Just . maybe [t] (t:)) (fromIntegral o))
     return (o, t)

findFreeBlock :: Size -> Heap (Maybe (Int, Offset))
findFreeBlock s = find <$> getM unused
  where find = fmap (fmap head . fst)
             . Im.minViewWithKey
             . snd
             . Im.split (fromIntegral s - 1)

