{-# LANGUAGE OverloadedStrings #-}

module Drawing where

import qualified Data.Map as Map
import qualified SDL
import qualified SDL.Image

import Control.Monad
import Control.Monad.Extra (whileM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int
import Foreign.C.Types (CInt)
import SDL (($=))

import Board
import Texturing
import qualified SDLUtils as U

scaleFactor :: CInt
scaleFactor = 3


drawTexture :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> m ()
drawTexture r t = SDL.copy r t Nothing Nothing


draw :: SDL.Renderer -> SDL.Rectangle CInt -> SDL.Texture -> IO ()
draw r p t = do
  setViewport r p
  drawTexture r t


toCIntPair :: (Int, Int) -> (CInt, CInt)
toCIntPair (x, y) = (fromIntegral x, fromIntegral y)


drawTexturePart :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> SDL.Rectangle CInt -> SDL.Rectangle CInt -> m ()
drawTexturePart r t ps pd = SDL.copy r t (Just ps) (Just (U.scaleRect scaleFactor pd))


drawObject :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> SDL.Rectangle CInt -> (CInt, CInt) -> CInt -> m ()
drawObject r t p (x, y) s = do
  let ts = tileSize * s
  drawTexturePart r t p $ U.mkRect (tileSize * x) (tileSize * y) ts ts


drawBoard :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> Board -> m ()
drawBoard r t board = do
  let (_,(n, m)) = bounds board
  forM_ [(i, j) | i <- [0..n], j <- [0..m]] $ \pos -> do
    let field = getField board pos
    unless (field == Forest) $ drawObject r t (getFieldRect field) (toCIntPair pos) 1

drawForest :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> Board -> m ()
drawForest r t board = do
  let (_,(n, m)) = bounds board
  forM_ [(i, j) | i <- [0..n], j <- [0..m]] $ \pos -> do
    let field = getField board pos
    when (field == Forest) $ drawObject r t (getFieldRect field) (toCIntPair pos) 1


drawTank :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> Tank -> m ()
drawTank r t tank = do
  let (x, y) = tPosition tank
  drawObject r t (getTankRect tank) (fromIntegral x, fromIntegral y) 2
  forM_ (tBullets tank) (drawBullet r t)


drawBullet :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> Bullet -> m ()
drawBullet r t b = drawTexturePart r t rect dst
  where
    (x, y) = bPosition b
    rect = getBulletRect b
    (xOffset, yOffset) = case bDirection b of
      UP -> (4, 0)
      LEFT -> (0, -4)
      DOWN -> (-4, 0)
      RIGHT -> (0, 4)
    dst = U.mkRect (tileSize * fromIntegral x + xOffset) (tileSize * fromIntegral y + yOffset) tileSize tileSize


drawBonus :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> Maybe (BonusItem, Position) -> m ()
drawBonus r t bi = case bi of
  Nothing -> return ()
  Just (b, pos) -> drawObject r t (getBonusRect b) (toCIntPair pos) 2


drawEagle :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> Eagle -> m ()
drawEagle r t e =
  drawObject r t (getEagleRect e) (toCIntPair eaglePosition) 2


drawScore :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> Int -> Int -> m ()
drawScore r t s p = do
  let d = s `mod` 10
  let ns = s `div` 10

  drawObject r t (getDigitRect d) (fromIntegral p, 0) 1
  unless (ns == 0) $ drawScore r t ns (p-1)

drawLives :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> (Map.Map Player Int) -> m ()
drawLives r t livesMap = do
  forM_ (Map.toAscList livesMap) drawLive

  where
    drawLive (Human pid, lives) = do
      drawObject r t (getDigitRect (pid + 1)) (0, fromIntegral pid * 2) 1
      drawObject r t (getDigitRect lives) (2, fromIntegral pid * 2) 1
    drawLive (_, _) = return ()


drawGame :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> GameState -> m ()
drawGame r t g = do
  let boardPos = U.mkRect 16 16 (26 * tileSize) (26 * tileSize)
  setViewport r boardPos

  drawEagle r t (gEagle g)
  drawBoard r t (gBoard g)
  forM_ (gTanks g) (drawTank r t)
  drawForest r t (gBoard g)
  drawBonus r t (gBonusItem g)

  let scorePos = U.mkRect (30 * tileSize) (2 * tileSize) (8 * tileSize) tileSize
  setViewport r scorePos
  drawScore r t (gPoints g) 7

  let livesPos = U.mkRect (30 * tileSize) (4 * tileSize) (3 * tileSize) (19 * tileSize)
  setViewport r livesPos
  drawLives r t (gLives g)


setViewport :: (MonadIO m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
setViewport r s = SDL.rendererViewport r $= Just (U.scaleRect scaleFactor s)
