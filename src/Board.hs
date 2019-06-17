{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Board where

import Data.Array
import Data.Binary
import Data.Bits
import Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy (ByteString, fromStrict)
import GHC.Generics (Generic)


type Position = (Int, Int)
type Velocity = (Int, Int)


-- Human and NPC tanks have different shapes
data Player = Human | NPC
  deriving (Binary, Generic, Show)

data Color = Yellow | Green | Silver
  deriving (Binary, Generic, Show)

data Size = Small | Medium | Large | Huge
  deriving (Binary, Generic, Show)

data TankBonus = Raft | Shield | Flashing
  deriving (Binary, Generic, Show)

data GeneralBonus = Bunker | Freeze
  deriving (Binary, Generic, Show)

data BonusItem = Helmet | Clock | Shovel | Star | Grenade | Life | Pistol | Boat
  deriving (Binary, Generic, Show)

data Field = Bricks | Forest | Stone | Ice | Empty
  deriving (Binary, Generic, Show)

data Dir = UP | DOWN | LEFT | RIGHT
  deriving (Binary, Generic, Show, Eq)

data Eagle = Alive | Dead
  deriving (Binary, Generic, Show)


data Bullet = Bullet
  { bDirection :: Dir
  , bPosition :: Position
  , bVelocity :: Velocity
  } deriving (Binary, Generic, Show)

data Tank = Tank
  { tDirection :: Dir
  , tPosition :: Position
  , tVelocity :: Velocity
  , tPlayer :: Player
  , tColor :: Color
  , tSize :: Size
  , tBonuses :: [TankBonus]
  , tBullets :: [Bullet]
  } deriving (Binary, Generic, Show)

type Board = Array (Int, Int) Field

-- TODO: [Tank] ==> (Map PlayerId Tank) ??
-- TODO: lives, points, enemies left, board number
data GameState = GameState
  { gBoard :: Board
  , gTanks :: [Tank]
  , gBonusItem :: Maybe (BonusItem, Position)
  , gGeneralBonuses :: [(GeneralBonus, Int)]
  , gEagle :: Eagle
  } deriving (Binary, Generic, Show)


newTank :: Player -> Int -> Tank
newTank pl i = case pl of
  Human -> let
    (x, col) = if i `mod` 2 == 0 then (8, Yellow) else (16, Green)
      in
    Tank UP (x, 24) (0, 0) pl col Small [] []
  NPC -> let
    x = case i `mod` 3 of
      0 -> 0
      1 -> 12
      2 -> 24
      in
    Tank DOWN (x, 0) (0, 0) pl Green Small [] []


initialGameState :: Board -> GameState
initialGameState board = GameState board tanks (Just (Helmet, (10, 10))) [] Alive
  where tanks = [newTank Human 0, newTank Human 1, newTank NPC 0, newTank NPC 1, newTank NPC 2]


x_coeff :: Int
x_coeff = 17

y_coeff :: Int
y_coeff = 67

randomField :: Int -> Int -> Field
randomField i j =
    case ((i * 1000 + j) `xor` 8475845) `mod` 5 of
        0 -> Bricks
        1 -> Forest
        2 -> Stone
        3 -> Ice
        _ -> Empty


getBoard :: Int -> Int -> Board
getBoard n m = array ((0,0),(n-1,m-1)) (concat [ [ ((i, j), Empty) | j <- [0..n-1] ] | i <- [0..m-1]] )

randomBoard :: Int -> Int -> Board
randomBoard n m = array ((0,0),(n-1,m-1)) (concat [ [ ((i, j), randomField i j) | j <- [0..n-1] ] | i <- [0..m-1]] )

decodeBoard :: BS.ByteString -> Board
decodeBoard str = decode $ fromStrict str

encodeGameState :: GameState -> Data.ByteString.Lazy.ByteString
encodeGameState gs =  encode gs

decodeGameState :: BS.ByteString -> GameState
decodeGameState str = decode $ fromStrict str
