
module Main where

import           Data.Array                     ( bounds
                                                , (!)
                                                , indices
                                                , listArray
                                                , Array
                                                )
import           Data.Foldable                  ( for_ )
import           Control.Monad                  ( when )

newtype Grid = Grid
  { unGrid :: Array (Int, Int) Int
  }
  deriving (Show, Eq)

data Direction
  = U
  | D
  | L
  | R

emptyGrid :: Grid
emptyGrid = Grid (listArray ((0, 0), (3, 3)) (repeat 0))

display :: Grid -> IO ()
display (Grid arr) = for_ (indices arr) $ \i -> do
    putStr (show (arr ! i))
    when (snd i == cB) (putChar '\n')
    where cB = snd $ snd (bounds arr)

main :: IO ()
main = pure ()
