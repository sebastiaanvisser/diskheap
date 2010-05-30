module Test where

import Control.Monad.Trans
import System.Heap.Heap
import Prelude hiding (read)

main :: IO ()
main =
  do e <- run "../test.db" $ 
       do p0 <- write "this is the first block"
          p1 <- write "and the second one"
          p2 <- write ((), False)
          p3 <- write (1234 :: Integer)
          writeRoot (p0, p1, p2, p3)
     print ("Result", e)

main2 :: IO ()
main2 = 
  do e <- run "../test.db" $ 
       do (p0, p1, p2, p3) <- readRoot
          v0 <- read p0
          v1 <- read p1
          v2 <- read p2
          v3 <- read p3
          liftIO $
            do print (v0 :: String)
               print (v1 :: String)
               print (v2 :: ((), Bool))
               print (v3 :: Integer)
     print ("Result: ", e)


