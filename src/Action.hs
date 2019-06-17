{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Action where

import qualified SDL

import Data.Aeson

import Data.Map
import Data.IORef
import Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy (ByteString, fromStrict)
import GHC.Generics (Generic)

import Debug.Trace

import Board

data GameAction =
      Move Dir
    | Shoot
    | NewPlayer
    | NoAction
    | InvalidAction
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

type MovesMap = Map Player GameAction

toAction :: SDL.EventPayload -> GameAction
toAction (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed False keysym)) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeLeft -> Move LEFT
    SDL.KeycodeRight -> Move RIGHT
    SDL.KeycodeUp -> Move UP
    SDL.KeycodeDown -> Move DOWN
    SDL.KeycodeSpace -> Shoot
    _ -> InvalidAction
toAction _ = InvalidAction

isAction :: SDL.EventPayload -> Bool
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

moveField :: GameState -> Dir -> Position -> Position
moveField gs dir pos =
	let newPos = moveByDir pos 1 dir in
	let maybeField = maybeGetField (gBoard gs) newPos  in
	let tanks = getTanksByPosition gs newPos in
	case maybeField of
		Just field -> if length tanks == 1 && canEnterField field then newPos else pos
		Nothing -> pos

moveFieldTank :: GameState -> Tank -> Tank
moveFieldTank gs tank =
	Tank
		(tDirection tank)
		(traceShowId (moveField gs (tDirection tank) (tPosition tank)))
		(tVelocity tank)
		(tPlayer tank)
		(tColor tank)
		(tSize tank)
		(tBonuses tank)
		(tBullets tank)

moveTank :: GameState -> Player -> Dir -> [Tank] -> [Tank]
moveTank _ _ _ [] = []
moveTank gs pl dir (tank:xs) =
	if tPlayer tank == pl
	then (moveFieldTank gs newTank):xs
	else tank:(moveTank gs pl dir xs)
	where newTank =
		Tank dir
		(tPosition tank)
		(tVelocity tank)
		(tPlayer tank)
		(tColor tank)
		(tSize tank)
		(tBonuses tank)
		(tBullets tank)

bulletVelocity ::Velocity
bulletVelocity = (4, 0)

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
			(barrelPosition tank)
			bulletVelocity
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
		Move d -> updateTanks (moveTank gs p d) gs
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

