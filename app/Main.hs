{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Array                     ( assocs
                                                , (//)
                                                , bounds
                                                , (!)
                                                , indices
                                                , listArray
                                                , Array
                                                )
import           Data.Foldable                  ( for_ )
import           Control.Monad                  ( when )
import           System.Random                  ( StdGen
                                                , getStdGen
                                                , RandomGen
                                                , randomR
                                                )
import           Control.Arrow                  ( ArrowApply(app)
                                                , Arrow(first)
                                                )
import           Graphics.Gloss                 ( play
                                                , text
                                                , translate
                                                , white
                                                , line
                                                , pictures
                                                , display
                                                , Display(InWindow)
                                                , Picture
                                                )
import           Data.Bifunctor                 ( Bifunctor(bimap, second) )
import           Graphics.Gloss.Interface.IO.Game
                                                ( Event(EventKey)
                                                , Key(Char)
                                                , KeyState(Down)
                                                )
import           System.Exit                    ( exitSuccess )
import           Control.Exception              ( catch
                                                , throw
                                                , Exception
                                                )

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

displayGrid :: Grid -> IO ()
displayGrid (Grid arr) = for_ (indices arr) $ \i -> do
    putStr (show (arr ! i))
    putChar $ if snd i == cB then '\n' else ' '
    where cB = snd $ snd (bounds arr)

score :: Grid -> Int
score = sum . unGrid

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
    displayGrid g
    final <- gameLoop gen g
    print "Game Over"
    putStrLn ("Score: " ++ show (score final))
  where
    gameLoop gen grid = if 0 `notElem` unGrid grid
        then return grid
        else do
            c <- getChar
            if c == 'q'
                then return grid
                else case keyDispatch c of
                    Nothing -> gameLoop gen grid
                    Just d ->
                        let grid'          = grid `move` d
                            (grid'', gen') = generateNum (grid', gen)
                        in  if grid' == grid then gameLoop gen grid else displayGrid grid'' >> gameLoop gen' grid''
    keyDispatch 'w' = return U
    keyDispatch 's' = return D
    keyDispatch 'a' = return L
    keyDispatch 'd' = return R
    keyDispatch _   = Nothing

data GraphicGrid = GraphicGrid
    { gHeight :: Int
    , gWidth  :: Int
    , blocks  :: [(Float, Float)] -- topleft points of blocks
    , ggen    :: StdGen
    }

graphicGrid :: Int -> Int -> StdGen -> GraphicGrid
graphicGrid h w = GraphicGrid h w bs where bs = [ (fromIntegral r, fromIntegral c) | r <- [0, h `div` 4 .. h - 1], c <- [0, w `div` 4 .. w - 1] ]

gridPicture :: GraphicGrid -> Picture
gridPicture gg = pictures (blockFrame <$> blocks gg)
  where
    blockFrame (r, c) = line [(r, c), (r + perHeight, c), (r + perHeight, c + perWidth), (r, c + perWidth), (r, c)]
    perHeight = fromIntegral (gHeight gg `div` 4)
    perWidth  = fromIntegral (gWidth gg `div` 4)

renderGrid :: GraphicGrid -> Grid -> Picture
renderGrid gg g = pictures (debuggingAnchor : gridPicture gg : numbers)
  where
    numbers   = app . bimap (uncurry translate . bimap ((perHeight *) . fromIntegral) ((perWidth *) . fromIntegral)) (text . show) <$> assocs (unGrid g)
    perHeight = fromIntegral (gHeight gg `div` 4)
    perWidth  = fromIntegral (gWidth gg `div` 4)

debuggingAnchor :: Picture
debuggingAnchor = pictures [line [(0, 0), (-100, 0)], line [(0, 0), (0, -100)]]

data GameExit = GameExit
    deriving Show

instance Exception GameExit

main :: IO ()
main = do
    gen <- getStdGen
    catch (play (InWindow "Nice Window" (800, 600) (100, 100)) white 30 (graphicGrid 800 600 gen, emptyGrid) (translate (-400) (-300) . uncurry renderGrid) handleEvent (const id))
        $ \case
              GameExit -> return ()
              e        -> throw e
  where
    handleEvent e world = case e of
        EventKey (Char c) Down _ _ -> dispatch c world
        _                          -> world
    dispatch key (gg, g) = case key of
        'q' -> throw GameExit
        c | c `elem` "wsad" -> let (g', gen) = generateNum (g `move` dispatchDirection c, ggen gg) in (gg { ggen = gen }, g')
          | otherwise       -> (gg, g)
    dispatchDirection 'w' = U
    dispatchDirection 's' = D
    dispatchDirection 'a' = L
    dispatchDirection 'd' = R
