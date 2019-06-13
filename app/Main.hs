{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Image
import qualified SDLUtils as U

import Control.Monad.Extra (whileM)
import Control.Monad.IO.Class (MonadIO)
import Foreign.C.Types (CInt)
import SDL (($=))
-- import SDL.Data.Texture (copyEx)


screenWidth :: CInt
screenWidth = 640


screenHeight :: CInt
screenHeight = 480


draw :: SDL.Renderer -> SDL.Rectangle CInt -> SDL.Texture -> IO ()
draw r p t = do
  setViewport r p
  drawTexture r t


topLeft :: SDL.Rectangle CInt
topLeft = U.mkRect 0 0 (screenWidth `div` 2) (screenHeight `div` 2)


topRight :: SDL.Rectangle CInt
topRight = U.mkRect (screenWidth `div` 2) 0 (screenWidth `div` 2) (screenHeight `div` 2)


drawTexture :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> m ()
drawTexture r t = SDL.copy r t Nothing Nothing


setViewport :: (MonadIO m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
setViewport r s = SDL.rendererViewport r $= Just s


main :: IO ()
main = U.withSDL $ U.withWindow "Lesson 03" (640, 480) $
  \w -> U.withRenderer w $ \r -> do

    SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
    t <- SDL.Image.loadTexture r "./assets/viewport.png"
    -- TODO: learn how to clip the texture
    -- t1 <- copyEx r t

    let doRender = SDL.clear r >> draw r topLeft t >> draw r topRight t >> SDL.present r

    whileM $
      U.isContinue <$> SDL.pollEvent
      >>= U.conditionallyRun doRender

    SDL.destroyTexture t
    -- SDL.destroyTexture t1
