{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Image
import qualified SDLUtils as U

import Control.Monad.Extra (whileM)
import Control.Monad.IO.Class (MonadIO)
import Foreign.C.Types (CInt)
import SDL (($=))

import Board
import Drawing


screenWidth :: CInt
screenWidth = 640


screenHeight :: CInt
screenHeight = 480


main :: IO ()
main = U.withSDL $ U.withWindow "Lesson 03" (640, 480) $
  \w -> U.withRenderer w $ \r -> do

    SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
    t <- SDL.Image.loadTexture r "./assets/tanks.png"

    let board = randomBoard 26 26
    let game = initialGameState board

    let doRender = SDL.clear r >> drawGame r t game >> SDL.present r

    whileM $
      U.isContinue <$> SDL.pollEvent
      >>= U.conditionallyRun doRender

    SDL.destroyTexture t
    -- SDL.destroyTexture t1
