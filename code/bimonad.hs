import Faceted
import Control.Monad(liftM, join)
import System.IO(IOMode(AppendMode, WriteMode))

prod = liftM join . swap

-- Syntax.
data Term =
    Var String                 -- Lambdas
  | Lam String Term
  | App Term Term
  | Const Value                -- Constants

-- Runtime data structures.
data RawValue =
    CharVal Char               -- Characters
  | LocVal (FIORef Value)      -- Mutable references
  | FnVal (Value -> Action)    -- Functions
type Value  = Faceted RawValue
type Action = FIO Value
type Env    = String -> Value

-- Helper functions.
extend :: Env -> String -> Value -> Env
extend e x v y | x == y    = v
               | otherwise = e y

-- Interpreter.
eval :: Env -> Term -> Action
eval e (Var x)     = return $ e x
eval e (Lam x t)   = return $ return $ FnVal $ \v -> eval (extend e x v) t
eval e (App t1 t2) = do v1 <- eval e t1             -- working in FIO monad
                        v2 <- eval e t2
                        prod $ do
                          FnVal f <- v1             -- working in Faceted monad
                          return $ f v2
eval e (Const v)   = return v

-- Constants.
makeHighSecurity :: RawValue
makeHighSecurity =
  FnVal $ \v ->
    return $ makeFaceted "H" v bottom
ref :: RawValue
ref =
  FnVal $ \v -> do                                  -- working in FIO monad
    ref <- newFIORef v
    return $ return $ LocVal ref
deref :: RawValue
deref =
  FnVal $ \v -> prod $ do                           -- working in Faceted monad
    LocVal ref <- v
    return $ readFIORef ref
assign :: RawValue
assign =
  FnVal $ \v1 ->
    return $ return $ FnVal $ \v2 -> prod $ do      -- working in Faceted monad
      LocVal ref <- v1
      rv2 <- v2
      return $ do                                   -- working in FIO monad
        writeFIORef ref v2
        return $ return rv2
printChar :: RawValue
printChar =
  FnVal $ \v -> prod $ do                           -- working in Faceted monad
    CharVal c <- v
    return $ do                                     -- working in FIO monad
      h <- openFileF [] "output.txt" AppendMode
      hPutCharF h (return c)
      hCloseF h
      return $ return $ CharVal c

------------------------------
-- Main program. (unimportant)

main = return ()

---------------------
-- Some of the figures
figure = figure where
  secret = undefined :: Faceted Int
  x = undefined :: FIORef (Faceted Int)
  figure =
    do prod $ do
         v <- secret
         return $
           if v == 42 then
             writeFIORef x (return 1)
           else
             return $ makePublic ()
       readFIORef x
  ifF :: Faceted Bool -> FIO (Faceted a) -> FIO (Faceted a) -> FIO (Faceted a)
  ifF facetedBool thenBranch elseBranch =
    prod (do v <- facetedBool
             return $ if v then thenBranch else elseBranch)
  figure_2 = do
    ifF (do { v <- secret; return $ v == 42 })
        (writeFIORef x (makePublic 1))
        (return (makePublic ()))
    readFIORef x
