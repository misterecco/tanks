{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Action where

import Data.Aeson

import Data.Map
import Data.IORef
import Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy (ByteString, fromStrict)
import GHC.Generics (Generic)
import Graphics.Vty
import Board

data GameAction =
      Move Dir
    | Shoot
    | NewPlayer
    | NoAction
    | InvalidAction
  deriving (Generic, Show, Eq, FromJSON, ToJSON)
  
type MovesMap = Map Player GameAction

toAction :: Event -> GameAction
toAction (EvKey KLeft _) = Move LEFT
toAction (EvKey KRight _) = Move RIGHT
toAction (EvKey KUp _) = Move UP
toAction (EvKey KDown _) = Move DOWN
toAction (EvKey (KChar ' ') _) = Shoot
toAction _ = InvalidAction

isAction :: Event -> Bool
isAction e = toAction e /= InvalidAction

encodeGameAction :: GameAction -> Data.ByteString.Lazy.ByteString
encodeGameAction gs = encode gs

decodeGameAction :: BS.ByteString -> GameAction
decodeGameAction str = 
	case decode $ fromStrict str of
	  Just x -> x
	  _ -> InvalidAction

updateMovesMap :: Player -> GameAction -> MovesMap -> MovesMap
updateMovesMap player gameAction movesMap =
	Data.Map.alter f player movesMap where
	f Nothing = Just gameAction
	f (Just NewPlayer) = Just NewPlayer
	f (Just _) = Just gameAction

addNewTank :: Player -> GameState -> GameState
addNewTank pl gs = 
	GameState
	(gBoard gs)
	(Tank DOWN (3, 0) (0, 0) pl Green Small [] [] : (gTanks gs))
	(gBonusItem gs)
    (gGeneralBonuses gs)
	(gEagle gs)

moveTank :: Player -> Dir -> [Tank] -> [Tank]
moveTank _ _ [] = []
moveTank pl dir (tank:xs) =
	if tPlayer tank == pl
	then newTank:xs
	else tank:(moveTank pl dir xs)
	where newTank =
		Tank dir 
		(tPosition tank)
		(tVelocity tank)
		(tPlayer tank)
		(tColor tank)
		(tSize tank)
		(tBonuses tank)
		(tBullets tank)
		
shootTank :: Player -> [Tank] -> [Tank]
shootTank _ [] = []
shootTank pl (tank:xs) =
	if tPlayer tank == pl
	then newTank:xs
	else tank:(shootTank pl xs)
	where newTank =
		Tank
		(tDirection tank)
		(tPosition tank)
		(tVelocity tank)
		(tPlayer tank)
		(tColor tank)
		(tSize tank)
		(tBonuses tank)
		((Bullet
			(tDirection tank)
			(tPosition tank)
			(tVelocity tank)
		):(tBullets tank))

updateTanks :: ([Tank] -> [Tank]) -> GameState -> GameState
updateTanks f gs = 
	GameState
	(gBoard gs)
	(f (gTanks gs))
	(gBonusItem gs)
    (gGeneralBonuses gs)
	(gEagle gs)
	

modifyMoves :: [(Player, GameAction)] -> GameState -> ([(Player, GameAction)], GameState)
modifyMoves [] gs = ([], gs)
modifyMoves ((p, action):xs) gs =
	let (ys, newGs) = modifyMoves xs newGS in ((p, NoAction):ys, newGs)
	where newGS = case action of
		NewPlayer -> addNewTank p gs
		Move d -> updateTanks (moveTank p d) gs
		NoAction -> gs
		Shoot -> updateTanks (shootTank p) gs

updateGameState :: GameState -> IORef MovesMap -> IO GameState
updateGameState gs movesMap = do {
	moves <- readIORef movesMap;
	let (newMoves, newGameState) = modifyMoves (Data.Map.toList moves) gs in do {
		writeIORef movesMap (Data.Map.fromList newMoves);
		return newGameState;
	}
}
	
