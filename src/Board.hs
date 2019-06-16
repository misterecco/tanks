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
import GHC.Generics (Rep, Generic)


type Position = (Int, Int)
type Velocity = (Int, Int)


-- Human and NPC tanks have different shapes
data Player = Human | NPC
  deriving (Binary, Generic, Show)

data Color = Yellow | Green | Silver
  deriving (Binary, Generic, Show)

data Size = Small | Medium | Large
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
-- TODO: eagle state
data GameState = GameState
  { gBoard :: Board
  , gTanks :: [Tank]
  , gBonusItem :: Maybe (BonusItem, Position)
  , gGeneralBonuses :: [(GeneralBonus, Int)]
  } deriving (Binary, Generic, Show)


-- TODO: calculate actual coordinates for each tank
-- TODO: randomness
-- TODO: record notation
newTank :: Player -> Int -> Tank
newTank pl _i = Tank UP (0, 0) (0, 0) pl Yellow Small [] []


-- TODO: record notation
initialGameState :: Board -> GameState
initialGameState board = GameState board [newTank Human 0] Nothing []


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
