module Texturing where

import qualified SDL
import Foreign.C.Types (CInt)

import Board
import qualified SDLUtils as U


tileSize :: CInt
tileSize = 8


getFieldRect :: Field -> SDL.Rectangle CInt
getFieldRect f = case f of
  Bricks -> toRect 256 64
  Forest -> toRect 264 72
  Stone -> toRect 256 72
  Ice -> toRect 272 72
  Empty -> toRect 280 72
  where
    toRect x y = U.mkRect x y tileSize tileSize


getTankRect :: Tank -> SDL.Rectangle CInt
getTankRect t = let
  sizeY = case tSize t of
    Small -> 0
    Medium -> 16
    Large -> 32
    Huge -> 48
  playerY = case tPlayer t of
    Human _ -> 0
    NPC _ -> 64
  directionX = case tDirection t of
    UP -> 0
    LEFT -> 32
    DOWN -> 64
    RIGHT -> 96
  (colorX, colorY) = case tColor t of
    Yellow -> (0, 0)
    Green -> (0, 128)
    Silver -> (128, 0)
  -- TODO: animation
  toRect x y = U.mkRect x y (2 * tileSize) (2 * tileSize)
    in
  toRect (directionX + colorX) (sizeY + playerY + colorY)


getEagleRect :: Eagle -> SDL.Rectangle CInt
getEagleRect e = case e of
  Alive -> toRect 304 32
  Dead -> toRect 320 32
  where
    toRect x y = U.mkRect x y (2 * tileSize) (2 * tileSize)

getBonusRect :: BonusItem -> SDL.Rectangle CInt
getBonusRect b = case b of
  Helmet -> toRect 256 112
  Clock -> toRect 272 112
  Shovel -> toRect 288 112
  Star -> toRect 304 112
  Grenade -> toRect 320 112
  Life -> toRect 336 112
  Pistol -> toRect 352 112
  Boat -> toRect 368 80 -- unfortunately, this texture is missing
  where
    toRect x y = U.mkRect x y (2 * tileSize) (2 * tileSize)