{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Heap.Pointer where

import Data.Bits
import Data.Binary

type Offset = Word64
type Size   = Word64

newtype Pointer a = Ptr { unPtr :: Offset }
  deriving (Show, Binary, Eq, Num, Bits)

nullPtr :: Pointer a
nullPtr = 0

pointerSize :: Word64
pointerSize = fromIntegral (bitSize nullPtr)

newtype Magic = Magic Word64
  deriving (Show, Binary, Eq, Num, Bits)

magic :: Magic
magic = 0xABCDEFABCDEF0123

magicSize :: Word64
magicSize = fromIntegral (bitSize magic)

newtype BlockHeader = BlockHeader Word64
  deriving (Show, Binary, Eq, Num, Bits)

blockHeaderSize :: Word64
blockHeaderSize = fromIntegral $ bitSize (undefined :: BlockHeader)

fileHeaderSize :: Word64
fileHeaderSize = magicSize

