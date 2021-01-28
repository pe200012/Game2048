
module Main where

import           Data.Array                     ( Array )

newtype Grid = Grid
  { unGrid :: Array (Int, Int) Int
  }
  deriving (Show, Eq)

data Direction
  = U
  | D
  | L
  | R

main :: IO ()
main = pure ()
