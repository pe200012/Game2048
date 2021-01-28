{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Array                     ( (//)
                                                , bounds
                                                , (!)
                                                , indices
                                                , listArray
                                                , Array
                                                )
import           Data.Foldable                  ( for_ )
import           Control.Monad                  ( when )
import           System.Random                  ( getStdGen
                                                , RandomGen
                                                , randomR
                                                )
import           Control.Arrow                  ( Arrow(first) )

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
    putChar $ if snd i == cB then '\n' else ' '
    where cB = snd $ snd (bounds arr)

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb d = mb >>= flip when d

move :: Grid -> Direction -> Grid
move (Grid arr) d = Grid $ fst
    (foldl
        (\(a, vis) i -> if a ! i == 0
            then (a, vis)
            else
                let (emptySlots, nonEmpty) = span ((== 0) . (a !)) (dispatch i d)
                in  case (null emptySlots, null nonEmpty, a ! head nonEmpty == a ! i) of
                        (True, True , _    )                           -> (a, vis)
                        (True, False, False)                           -> (a, vis)
                        (_, False, True) | head nonEmpty `notElem` vis -> (a // [(head nonEmpty, 2 * a ! i), (i, 0)], head nonEmpty : vis)
                        (False, True , _    )                          -> (a // [(last emptySlots, a ! i), (i, 0)], vis)
                        (False, False, False)                          -> (a // [(last emptySlots, a ! i), (i, 0)], vis)
                        _                                              -> (a, vis)
        )
        (arr, [])
        loopArrangement
    )
  where
    dispatch (r, c) U = [ (r1, c) | r1 <- [r - 1, r - 2 .. 0] ]
    dispatch (r, c) D = [ (r1, c) | r1 <- [r + 1 .. fst (snd (bounds arr))] ]
    dispatch (r, c) L = [ (r, c1) | c1 <- [c - 1, c - 2 .. 0] ]
    dispatch (r, c) R = [ (r, c1) | c1 <- [c + 1 .. snd (snd (bounds arr))] ]
    loopArrangement = case d of
        U -> indices arr
        L -> indices arr
        D -> reverse (indices arr)
        R -> reverse (indices arr)

generateNum :: RandomGen g => (Grid, g) -> (Grid, g)
generateNum origin@(Grid arr, g) = if null emptySlots then origin else (Grid (arr // [(p, num)]), g'')
  where
    emptySlots = filter ((== 0) . (arr !)) (indices arr)
    (p  , g' ) = first (emptySlots !!) $ randomR (0, length emptySlots - 1) g
    (num, g'') = first (\(n :: Double) -> if n > 0.9 then 4 else 2) $ randomR (0, 1.0) g'

newGame :: IO ()
newGame = do
    (g, gen) <- generateNum . (emptyGrid, ) <$> getStdGen
    display g
    gameLoop gen g
  where
    gameLoop gen grid = if 0 `notElem` unGrid grid
        then return ()
        else do
            c <- getChar
            if c == 'q'
                then return ()
                else case keyDispatch c of
                    Nothing -> gameLoop gen grid
                    Just d ->
                        let grid'          = grid `move` d
                            (grid'', gen') = generateNum (grid', gen)
                        in  if grid' == grid then gameLoop gen grid else display grid'' >> gameLoop gen' grid''
    keyDispatch 'w' = return U
    keyDispatch 's' = return D
    keyDispatch 'a' = return L
    keyDispatch 'd' = return R
    keyDispatch _   = Nothing

main :: IO ()
main = newGame
