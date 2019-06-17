{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Board where

import Data.Aeson

import qualified Data.Array

import Data.List
import Data.Map
import Data.Bits
import Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy (ByteString, fromStrict)
import GHC.Generics (Generic, Rep)


type Position = (Int, Int)
type Velocity = (Int, Int)


-- Human and NPC tanks have different shapes
data Player = Human Int | NPC Int
  deriving (Generic, Show, FromJSON, ToJSON, Eq)

instance Ord Player where
	(<=) (Human _) (NPC _) = True
	(<=) (NPC _) (Human _) = False
	(<=) (Human a) (Human b) = a <= b
	(<=) (NPC a) (NPC b) = a <= b

data Color = Yellow | Green | Silver
  deriving (Generic, Show, FromJSON, ToJSON)

data Size = Small | Medium | Large | Huge
  deriving (Generic, Show, FromJSON, ToJSON)

data TankBonus = Raft | Shield | Flashing
  deriving (Generic, Show, FromJSON, ToJSON)

data GeneralBonus = Bunker | Freeze
  deriving (Generic, Show, FromJSON, ToJSON)

data BonusItem = Helmet | Clock | Shovel | Star | Grenade | Life | Pistol | Boat
  deriving (Generic, Show, FromJSON, ToJSON)

data Field = Bricks | Forest | Stone | Ice | Empty
  deriving (Generic, Show, FromJSON, ToJSON)

data Dir = UP | DOWN | LEFT | RIGHT
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

data Eagle = Alive | Dead
  deriving (Generic, Show, FromJSON, ToJSON)


data Bullet = Bullet
  { bDirection :: Dir
  , bPosition :: Position
  , bVelocity :: Velocity
  } deriving (Generic, Show, FromJSON, ToJSON)

data Tank = Tank
  { tDirection :: Dir
  , tPosition :: Position
  , tVelocity :: Velocity
  , tPlayer :: Player
  , tColor :: Color
  , tSize :: Size
  , tBonuses :: [TankBonus]
  , tBullets :: [Bullet]
  } deriving (Generic, Show, FromJSON, ToJSON)

data Board = Board Int Int (Map (Int, Int) Field)
	deriving (Generic, Show, FromJSON, ToJSON)

-- TODO: [Tank] ==> (Map PlayerId Tank) ??
-- TODO: lives, points, enemies left, board number
data GameState = GameState
  { gBoard :: Board
  , gTanks :: [Tank]
  , gBonusItem :: Maybe (BonusItem, Position)
  , gGeneralBonuses :: [(GeneralBonus, Int)]
  , gEagle :: Eagle
  } deriving (Generic, Show, FromJSON, ToJSON)


newTank :: Player -> Tank
newTank pl = case pl of
  Human i -> let
    (x, col) = if i `mod` 2 == 0 then (8, Yellow) else (16, Green)
      in
    Tank UP (x, 24) (0, 0) pl col Small [] []
  NPC i -> let
    x = case i `mod` 3 of
      0 -> 0
      1 -> 12
      2 -> 24
      in
    Tank DOWN (x, 0) (0, 0) pl Green Small [] []


initialGameState :: Board -> GameState
initialGameState board = GameState board tanks (Just (Helmet, (10, 10))) [] Alive
  where tanks = [newTank $ Human 0, newTank $ Human 1, newTank $ NPC 0, newTank $ NPC 1, newTank $ NPC 2]


x_coeff :: Int
x_coeff = 17

y_coeff :: Int
y_coeff = 67

bounds :: Board -> ((Int, Int), (Int, Int))
bounds (Board n m _ ) = ((0, 0), (n-1, m-1))

-- FIELD FUNCTIONS --

randomField :: Int -> Int -> Field
randomField i j =
    case ((i * 1000 + j) `xor` 8475845) `mod` 5 of
        0 -> Bricks
        1 -> Forest
        2 -> Stone
        3 -> Ice
        _ -> Empty

getField :: Board -> (Int, Int) -> Field
getField (Board _ _ mapa) pos = mapa ! pos

maybeGetField :: Board -> (Int, Int) -> Maybe Field
maybeGetField (Board _ _ mapa) pos = Data.Map.lookup pos mapa

canEnterField :: Field -> Bool
canEnterField Bricks = False 
canEnterField Forest = True
canEnterField Stone = False
canEnterField Ice = True
canEnterField Empty = True

-- DIR FUNCTIONS --
moveByDir :: Position -> Dir -> Position
moveByDir (x, y) UP = (x, y - 2)
moveByDir (x, y) DOWN = (x, y + 2)
moveByDir (x, y) RIGHT = (x + 2, y)
moveByDir (x, y) LEFT = (x - 2, y)

getBoard :: Int -> Int -> Board
getBoard n m = Board n m $ Data.Map.fromList (concat [ [ ((i, j), Empty) | j <- [0..n-1] ] | i <- [0..m-1]] )

randomBoard :: Int -> Int -> Board
randomBoard n m = Board n m $ Data.Map.fromList (concat [ [ ((i, j), randomField i j) | j <- [0..n-1] ] | i <- [0..m-1]] )

encodeGameState :: GameState -> Data.ByteString.Lazy.ByteString
encodeGameState gs =  encode $ gs

decodeGameState :: BS.ByteString -> GameState
decodeGameState str =
	case decode $ fromStrict str of
	  Just x -> x
	  _ -> error "Invalid Game State"
