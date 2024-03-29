{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Board where

import Data.Aeson

import qualified Data.List as List

import Data.Map
import Data.Bits
import Data.Maybe
import Control.Monad.State
import Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy (ByteString, fromStrict)
import GHC.Generics (Generic, Rep)

import Debug.Trace

type Position = (Int, Int)
type Velocity = (Int, Int)


-- Human and NPC tanks have different shapes
data Player = Human Int | NPC Int
  deriving (Generic, Show, FromJSON, ToJSON, Eq, FromJSONKey, ToJSONKey)

instance Ord Player where
  (<=) (Human _) (NPC _) = True
  (<=) (NPC _) (Human _) = False
  (<=) (Human a) (Human b) = a <= b
  (<=) (NPC a) (NPC b) = a <= b

data Color = Yellow | Green | Silver
  deriving (Generic, Show, FromJSON, ToJSON, Eq)

data Size = Small | Medium | Large | Huge
  deriving (Generic, Show, FromJSON, ToJSON, Eq)

data TankBonus = Raft | Shield | Flashing
  deriving (Generic, Show, FromJSON, ToJSON, Eq)

data GeneralBonus = Bunker | Freeze
  deriving (Generic, Show, FromJSON, ToJSON, Eq)

data BonusItem = Helmet | Clock | Shovel | Star | Grenade | Life | Pistol | Boat
  deriving (Generic, Show, FromJSON, ToJSON, Eq)

data Field = Bricks | Forest | Stone | Ice | Empty
  deriving (Generic, Show, FromJSON, ToJSON, Eq)

data Dir = UP | DOWN | LEFT | RIGHT
  deriving (Generic, Show, ToJSON, FromJSON, Eq)

data Eagle = Alive | Dead
  deriving (Generic, Show, FromJSON, ToJSON, Eq)

data TankStatus = Working | Destroyed
  deriving (Generic, Show, FromJSON, ToJSON, Eq)

data Bullet = Bullet
  { bDirection :: Dir
  , bPosition :: Position
  , bVelocity :: Velocity
  , bPlayer :: Player
  } deriving (Generic, Show, FromJSON, ToJSON, Eq)

data Tank = Tank
  { tDirection :: Dir
  , tPosition :: Position
  , tVelocity :: Velocity
  , tPlayer :: Player
  , tColor :: Color
  , tSize :: Size
  , tBonuses :: [TankBonus]
  , tBullets :: [Bullet]
  , tStatus :: TankStatus
  } deriving (Generic, Show, FromJSON, ToJSON, Eq)

data Board = Board Int Int (Map (Int, Int) Field)
  deriving (Generic, Show, FromJSON, ToJSON)

data GameState = GameState
  { gBoard :: Board
  , gTanks :: [Tank]
  , gBonusItem :: Maybe (BonusItem, Position)
  , gGeneralBonuses :: [(GeneralBonus, Int)]
  , gEagle :: Eagle
  , gPoints :: Int
  , gLives :: Map Player Int
  } deriving (Generic, Show, FromJSON, ToJSON)

type GameStateM a = State GameState a

initialGameState :: Board -> GameState
initialGameState board = GameState board [] Nothing [] Alive 0 Data.Map.empty

eaglePosition :: Position
eaglePosition = (12, 24)

bounds :: Board -> ((Int, Int), (Int, Int))
bounds (Board n m _ ) = ((0, 0), (n-1, m-1))

-- PLAYER FUNCTIONS --

sameTeam :: Player -> Player -> Bool
sameTeam (Human _) (Human _) = True
sameTeam (NPC _) (NPC _) = True
sameTeam _ _ = False

isHuman :: Player -> Bool
isHuman (Human _ ) = True
isHuman (NPC _) = False

-- TANK FUNCTIONS --

newPlayerTank :: Int -> Tank
newPlayerTank i =
    let (x, col) = if i `mod` 2 == 0 then (8, Green) else (16, Green)
      in
    Tank UP (x, 24) (0, 0) (Human i) col Small [] [] Working

newNPCTank :: Int -> Int -> Int -> Tank
newNPCTank i x r = let
  color = case r `mod` 3 of
      0 -> Yellow
      1 -> Green
      2 -> Silver
  size = case r `mod` 4 of
      0 -> Small
      1 -> Medium
      2 -> Large
      3 -> Huge
    in
    Tank DOWN (x, 0) (0, 0) (NPC i) color size [] [] Working

barrelPosition :: Tank -> Position
barrelPosition tank =
  let (x, y) = tPosition tank in
  case tDirection tank of
    UP -> (x, y)
    DOWN -> (x + 1, y + 1)
    LEFT -> (x, y + 1)
    RIGHT -> (x + 1, y)

tankOverlap :: Position -> Tank -> Bool
tankOverlap (x, y) tank =
  let (tx, ty) = tPosition tank in
  ((abs $ tx - x) <= 1) && ((abs $ ty - y) <= 1)

tankOverlapBulletP :: Position -> Tank -> Bool
tankOverlapBulletP (x, y) tank =
  let (tx, ty) = tPosition tank in
  -1 <= tx - x && tx - x <= 0 && -1 <= ty - y && ty - y <= 0

tankPositions :: Tank -> [Position]
tankPositions tank =
  let (x, y) = tPosition tank in
  [(x, y), (x + 1, y), (x + 1, y + 1), (x, y + 1)]

tankOverlapBullet :: Bullet -> Tank -> Bool
tankOverlapBullet bullet tank =
  let bulletPos = bulletPositions bullet in
  let tankPos = tankPositions tank in
  List.intersect bulletPos tankPos /= []

-- COLOR FUNCTIONS --

nextColor :: Color -> Maybe Color
nextColor Green = Just Yellow
nextColor Yellow = Just Silver
nextColor Silver = Nothing

-- BULLET FUNCTIONS --

bulletPositions :: Bullet -> [Position]
bulletPositions bullet =
    let (x, y) = bPosition bullet in
    let second = case bDirection bullet of
                    UP -> (x + 1, y)
                    DOWN -> (x - 1, y)
                    LEFT -> (x, y - 1)
                    RIGHT -> (x, y + 1)
    in [bPosition bullet, second]

bulletOverlap :: Position -> Bullet -> Bool
bulletOverlap pos bullet =
    isJust $ List.find (== pos) (bulletPositions bullet)

-- FIELD FUNCTIONS --

allFieldsBoard :: Board -> [Position]
allFieldsBoard (Board _ _ board) = keys $ board

allFields :: GameState -> [Position]
allFields gs = allFieldsBoard $ gBoard gs

tankOverlapField :: Position -> Position -> Bool
tankOverlapField (x, y) (tx, ty) =
  -1 <= tx - x && tx - x <= 0 && -1 <= ty - y && ty - y <= 0

getFieldsByBullet :: Board -> Bullet -> [(Position, Field)]
getFieldsByBullet (Board _ _ mapa) bullet =
    Data.Map.toList $ Data.Map.filterWithKey (\k -> \v -> bulletOverlap k bullet && not (canEnterField v)) mapa

getFieldsByTank :: Board -> (Int, Int) -> [Field]
getFieldsByTank (Board _ _ mapa) pos =
    elems $ Data.Map.filterWithKey (\k -> \v -> tankOverlapField k pos) mapa

getField :: Board -> (Int, Int) -> Field
getField (Board _ _ mapa) pos =
  case Data.Map.lookup pos mapa of
    Just x -> x
    Nothing -> traceShow pos Empty

maybeGetField :: Board -> (Int, Int) -> Maybe Field
maybeGetField (Board _ _ mapa) pos = Data.Map.lookup pos mapa

canEnterField :: Field -> Bool
canEnterField Bricks = False
canEnterField Forest = True
canEnterField Stone = False
canEnterField Ice = True
canEnterField Empty = True

-- DIR FUNCTIONS --
moveByDir :: Position -> Int -> Dir -> Position
moveByDir (x, y) vel UP = (x, y - vel)
moveByDir (x, y) vel DOWN = (x, y + vel)
moveByDir (x, y) vel RIGHT = (x + vel, y)
moveByDir (x, y) vel LEFT = (x - vel, y)

-- BOARD FUNCTIONS --

firstLevel :: Board
firstLevel = Board 26 26 $ Data.Map.fromList $ concat
  [
    [((i, j), Empty) | i <- [0..25], j <- [0..25]],
    [((i, j), Forest) | i <- [0..1], j <- [16..25]],
    [((i, j), Forest) | i <- [24..25], j <- [16..25]],
    [((i, j), Forest) | i <- [2..5], j <- [24..25]],
    [((i, j), Forest) | i <- [20..23], j <- [24..25]],
    [((i, j), Ice) | i <- [2..11], j <- [0..1]],
    [((i, j), Ice) | i <- [14..23], j <- [0..1]],
    [((i, j), Ice) | i <- [0..25], j <- [12..13]],
    [((i, j), Ice) | i <- [0..3], j <- [2..11]],
    [((i, j), Ice) | i <- [22..25], j <- [2..11]],
    [((i, j), Stone) | i <- [0..1], j <- [14..15]],
    [((i, j), Stone) | i <- [12..13], j <- [14..15]],
    [((i, j), Stone) | i <- [24..25], j <- [14..15]],
    [((i, j), Stone) | i <- [4..5], j <- [18..19]],
    [((i, j), Stone) | i <- [8..9], j <- [18..19]],
    [((i, j), Stone) | i <- [16..17], j <- [18..19]],
    [((i, j), Stone) | i <- [20..21], j <- [18..19]],
    [((i, j), Bricks) | i <- [6..11], j <- [2..3]],
    [((i, j), Bricks) | i <- [14..19], j <- [2..3]],
    [((i, j), Bricks) | i <- [6..7], j <- [4..5]],
    [((i, j), Bricks) | i <- [10..11], j <- [4..5]],
    [((i, j), Bricks) | i <- [14..15], j <- [4..5]],
    [((i, j), Bricks) | i <- [18..19], j <- [4..5]],
    [((i, j), Bricks) | i <- [6..11], j <- [6..7]],
    [((i, j), Bricks) | i <- [14..15], j <- [6..7]],
    [((i, j), Bricks) | i <- [18..19], j <- [6..7]],
    [((i, j), Bricks) | i <- [10..11], j <- [8..9]],
    [((i, j), Bricks) | i <- [14..15], j <- [8..9]],
    [((i, j), Bricks) | i <- [18..19], j <- [8..9]],
    [((i, j), Bricks) | i <- [6..11], j <- [10..11]],
    [((i, j), Bricks) | i <- [14..19], j <- [10..11]],
    [((i, j), Bricks) | i <- [2..3], j <- [17..23]],
    [((i, j), Bricks) | i <- [6..7], j <- [17..23]],
    [((i, j), Bricks) | i <- [18..19], j <- [17..23]],
    [((i, j), Bricks) | i <- [22..23], j <- [17..23]],
    [((i, j), Bricks) | i <- [10..11], j <- [15..20]],
    [((i, j), Bricks) | i <- [12..13], j <- [16..19]],
    [((i, j), Bricks) | i <- [14..15], j <- [15..20]],
    [((i, j), Bricks) | i <- [11, 14], j <- [23..25]],
    [((i, j), Bricks) | i <- [12..13], j <- [23]]
  ]

getBoard :: Int -> Int -> Board
getBoard n m = Board n m $ Data.Map.fromList (concat [ [ ((i, j), Empty) | j <- [0..n-1] ] | i <- [0..m-1]] )

-- GAMESTATE FUNCTIONS --

increasePoints :: Int -> GameStateM ()
increasePoints x = do
  gs <- get
  put $ gs { gPoints = x + gPoints gs }

getTanksByTankPosition :: GameState -> Position -> [Tank]
getTanksByTankPosition gs pos =
  List.filter (tankOverlap pos) (gTanks gs)

encodeGameState :: GameState -> Data.ByteString.Lazy.ByteString
encodeGameState gs =  encode $ gs

decodeGameState :: BS.ByteString -> GameState
decodeGameState str =
  case decode $ fromStrict str of
    Just x -> x
    _ -> error "Invalid Game State"
