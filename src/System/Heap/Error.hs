module System.Heap.Error where

import Control.Monad.Trans.Error

data HeapError =
    InvalidHeapMagic
  | UnknownError String
  | IOError      String
  deriving Show

instance Error HeapError where
  noMsg  = UnknownError ""
  strMsg = UnknownError 

