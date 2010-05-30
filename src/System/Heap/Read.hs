{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  #-}
module System.Heap.Read where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Error
import Data.Binary
import Data.ByteString.Lazy
import System.Heap.Error
import System.Heap.Pointer
import Prelude hiding (read)
import System.IO

newtype Heap a = Heap (ReaderT Handle (ErrorT HeapError IO) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Handle
    , MonadError  HeapError
    )

run :: Handle -> Heap a -> IO (Either HeapError a)
run h (Heap c) = runErrorT (runReaderT c h)

read :: Binary a => Pointer a -> Heap a
read (Ptr off) =
  do h <- ask
     liftIO $
       do hSeek h AbsoluteSeek (fromIntegral off)
          x :: Word64     <- decode <$> hGet h 8
          b :: ByteString <- decode <$> hGet h (8 + fromIntegral x)
          return (decode b)

