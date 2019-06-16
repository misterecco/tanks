{-# LANGUAGE OverloadedStrings #-}

module Drawing where

import qualified Data.Map as Map
import qualified SDL
import qualified SDL.Image

import Control.Monad
import Control.Monad.Extra (whileM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Array
import Data.Int
import Foreign.C.Types (CInt)
import SDL (($=))

import Board
import Texturing
import qualified SDLUtils as U


drawTexture :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> m ()
drawTexture r t = SDL.copy r t Nothing Nothing


draw :: SDL.Renderer -> SDL.Rectangle CInt -> SDL.Texture -> IO ()
draw r p t = do
  setViewport r p
  drawTexture r t


drawTexturePart :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> SDL.Rectangle CInt -> SDL.Rectangle CInt -> m ()
drawTexturePart r t ps pd = SDL.copy r t (Just ps) (Just pd)


drawObject :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> SDL.Rectangle CInt -> (CInt, CInt) -> CInt -> m ()
drawObject r t p (x, y) s = do
  let ts = tileSize * s
  drawTexturePart r t p $ U.mkRect (tileSize * x) (tileSize * y) ts ts


drawBoard :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> Board -> m ()
drawBoard r t board = do
  let (_,(n, m)) = bounds board
  let boardPos = U.mkRect (16 :: CInt) 16 ((fromIntegral (n+1)) * tileSize) ((fromIntegral (m+1)) * tileSize)
  setViewport r boardPos

  forM_ [(i, j) | i <- [0..n], j <- [0..m]] $ \pos@(x, y) -> do
    drawObject r t (getFieldRect (board ! pos)) (fromIntegral x, fromIntegral y) 1


drawTank :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> Tank -> m ()
drawTank r t tank = do
  let (x, y) = tPosition tank
  drawObject r t (getTankRect tank) (fromIntegral x, fromIntegral y) 2
  -- forM_ (tBullets tank) (drawBullet r t)


-- TODO: implement
drawBullet :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> Bullet -> m ()
drawBullet = undefined


drawBonus :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> Maybe (BonusItem, Position) -> m ()
drawBonus r t bi = case bi of
  Nothing -> return ()
  -- TODO: implement
  Just (b, pos) -> return ()


-- TODO: use this instead of drawBoard
-- TODO: draw eagle
drawGame :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> GameState -> m ()
drawGame r t g = do
  drawBoard r t (gBoard g)
  -- drawBonus r t (gBonusItem g)
  forM_ (gTanks g) (drawTank r t)


setViewport :: (MonadIO m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
setViewport r s = SDL.rendererViewport r $= Just s
