module Misc where

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import System.IO.Unsafe

my_fib_unsafe :: Integer -> Integer
my_fib_unsafe a = unsafePerformIO $ do
    ref <- newIORef $ M.fromList [(0,0),(1,1)]
    fib a ref
  where    
    fib :: Integer -> IORef (Map Integer Integer) -> IO Integer
    fib n ref = do
        memo <- readIORef ref
        case M.lookup n memo of
            Just z  -> return z
            Nothing -> do
                x <- fib (n - 2) ref
                y <- fib (n - 1) ref
                let z = x + y
                modifyIORef ref (M.insert n z)
                return z

my_fib_io :: Integer -> IO Integer
my_fib_io a = do
    ref <- newIORef $ M.fromList [(0,0),(1,1)]
    fib a ref
  where    
    fib :: Integer -> IORef (Map Integer Integer) -> IO Integer
    fib n ref = do
        memo <- readIORef ref
        case M.lookup n memo of
            Just z  -> return z
            Nothing -> do
                x <- fib (n - 2) ref
                y <- fib (n - 1) ref
                let z = x + y
                modifyIORef ref (M.insert n z)
                return z
