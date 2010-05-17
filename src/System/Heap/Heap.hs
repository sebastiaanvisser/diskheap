{-# LANGUAGE FlexibleContexts #-}
module System.Heap.Heap where

import Control.Applicative
import Control.Monad.Error
import Data.Binary
import System.IO
import Control.Exception
import System.Heap.Pointer
import System.Heap.Error
import qualified Data.ByteString.Lazy as Lazy
import qualified System.Heap.Alloc as Alloc
import qualified System.Heap.Read  as Read
import qualified System.Heap.Write as Write


run :: FilePath -> Write.Heap a -> IO (Either HeapError a)
run f comp =
  do me <- try $
       withBinaryFile f ReadWriteMode $ \h -> runner h $
         do initialize h
            Write.readAllocationMap
            a <- comp
            Write.writeAllocationMap
            return a
     case me of
       Left  e -> return (Left (IOError (show (e :: SomeException))))
       Right r -> return r
  where runner h = Read.run h . Alloc.run . Write.run


initialize :: Handle -> Write.Heap ()
initialize h =
  do fs <- liftIO (hFileSize h)
     if fs == 0
       then do liftIO (Lazy.hPut h (encode magic))
               Write.writeAllocationMap
       else do m <- decode <$> liftIO (Lazy.hGet h 8)
               when (m == magic) (throwError InvalidHeapMagic)

main :: IO ()
main =
  do x <- run "../test.db" $ liftIO (print "xxx")
     print ("xx", x)

