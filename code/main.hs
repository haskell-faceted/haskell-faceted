import Faceted
import Control.Monad(liftM, join)
import System.IO(IOMode(WriteMode))

prod = liftM join . swap

rep :: Monad m => m a -> Int -> m a
rep x 1 = x
rep x n = do
  x
  rep x (n-1)

------------------------------------------------------------------------
-- Experiment 1: Faceted monad
--   Quadratic complexity (worse than expected)

x0 = makeFacets "k" 'a' 'b'
x1 = rep x0 (ceiling $ n*3475)
x2 = rep x1 (ceiling $ n*3475)
main_x1 = flip runFIO [] $ do
  h <- openFileF [] "output.txt" WriteMode
  hPutCharF h x2
  hCloseF h
main_x2 =
  flip runFIO [] $ do
    prod $ do
      c <- x2
      return $ do
        h <- openFileF [] "output.txt" WriteMode
        hPutCharF h (makePublic c)
        hCloseF h
        return $ makePublic ()

------------------------------------------------------------------------
-- Experiment 2: IO monad
--   Quadratic complexity (unfortunate, but expected)

y0 = return ()
y1 = rep y0 (ceiling $ n*6750)
y2 = rep y1 (ceiling $ n*6750)
main_y = do
  y2
  print "foo"

------------------------------------------------------------------------
-- Experiment 3: Totally pure code
--   Linear complexity (as expected)

pure_rep m = f 0 where
  f acc 0 = acc
  f acc n = (f $! (m + acc)) $! (n-1)
z0 = 1
z1 = pure_rep z0 (ceiling $ n*4097000)
z2 = pure_rep z1 (ceiling $ n*4097000)
main_z = do
  print z2

------------------------------------------------------------------------
-- Experiment 4: Maybe monad
--   Linear complexity (as expected)

m0 = Just "foo"
m1 = rep m0 (ceiling $ n*13808000)
m2 = rep m1 (ceiling $ n*13808000)
main_m = do
  print m2

------------------------------------------------------------------------
-- Main: Run all the experiments

n = 2
main = do
  main_x1
