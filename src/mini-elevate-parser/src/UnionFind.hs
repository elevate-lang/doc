module UnionFind (
  Point, fresh, repr, find, find', change, modify, union, equivalent, redundant
) where

import Data.IORef
import Control.Monad
import Control.Exception

data Link a = 
  Info {weight :: IORef Int, descriptor :: IORef a} | 
  Link {link :: IORef (Link a)}
  deriving (Eq)

type Point a = IORef (Link a)

fresh :: a -> IO (Point a)
fresh desc = newIORef =<< (Info <$> newIORef 1 <*> newIORef desc)

repr :: Point a -> IO (Point a)
repr point = do
  p <- readIORef point
  case p of
    Link p' -> do
      p'' <- repr p'
      when (p'' /= p') (writeIORef point =<< readIORef p')
      return p''
    Info {} -> return point

find :: Point a -> IO (IORef a)
find point = do
  p <- readIORef point
  case p of
    Info {} -> return (descriptor p)
    Link p' -> do
      p'' <- readIORef p'
      case p'' of
        Info {} -> return (descriptor p'')
        _ -> find =<< repr point

find' :: Point a -> IO a
find' p = readIORef =<< find p

change :: Point a -> a -> IO (IORef a)
change point v = do
  desc <- find point
  writeIORef desc v
  return desc

modify :: Point a -> (a -> a) -> IO (IORef a)
modify point f = do
  desc <- find point
  writeIORef desc =<< (f <$> readIORef desc)
  return desc

union :: Point a -> Point a -> IO ()
union point1 point2 = do
  p1r <- repr point1
  p2r <- repr point2
  if p1r == p2r then
    throwIO (AssertionFailed "union-find: cannt union two points belonging to the same class")
  else do
    p1 <- readIORef p1r
    p2 <- readIORef p2r
    case (p1, p2) of
      (Info {}, Info {}) -> do
        w1 <- readIORef (weight p1)
        w2 <- readIORef (weight p2)
        if w1 >= w2 then do
          writeIORef p2r (Link p1r)
          writeIORef (weight p1) (w1 + w2)
          writeIORef (descriptor p1) =<< readIORef (descriptor p2)
        else do
          writeIORef p1r (Link p2r)
          writeIORef (weight p2) (w1 + w2)
      _ -> throwIO (AssertionFailed "union-find: impossible")

equivalent :: Point a -> Point a -> IO Bool
equivalent point1 point2 = (==) <$> repr point1 <*> repr point2

redundant :: Point a -> IO Bool
redundant point = do
  p <- readIORef point
  case p of
    Link {} -> return True
    Info {} -> return False
