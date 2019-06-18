{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Action where

import qualified SDL
import qualified Data.List as List

import Data.Aeson

import Data.Maybe
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
addNewTank pl gs = let
  oldTanks = gTanks gs
  newTank = case pl of
    (Human pid) -> newPlayerTank pid
    -- TODO: randomness, make sure the field is available
    (NPC pid) -> newNPCTank pid 0 0
  in
    gs {
      gTanks = (newTank : oldTanks)
    }

moveField :: GameState -> Dir -> Position -> Position
moveField gs dir pos =
	let newPos = moveByDir pos 1 dir in
	let fields = getFieldsByTank (gBoard gs) newPos in
	let tanks = getTanksByTankPosition gs newPos in
	if length fields == 4 && length tanks == 1 && all canEnterField fields then newPos else pos

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
			(tPlayer tank)
		):(tBullets tank))

updateBullets :: Tank -> [Bullet] -> Tank
updateBullets tank bullets =
    Tank
	(tDirection tank)
	(tPosition tank)
	(tVelocity tank)
	(tPlayer tank)
	(tColor tank)
	(tSize tank)
	(tBonuses tank)
	bullets

updateTanks :: ([Tank] -> [Tank]) -> GameState -> GameState
updateTanks f gs =
	GameState
	(gBoard gs)
	(f (gTanks gs))
	(gBonusItem gs)
    (gGeneralBonuses gs)
	(gEagle gs)

updateFieldsBoard :: (Map Position Field -> Map Position Field) -> Board -> Board
updateFieldsBoard f (Board n m b) = Board n m (f b)

updateFields :: (Map Position Field -> Map Position Field) -> GameState -> GameState
updateFields f gs =
	GameState
	(updateFieldsBoard f (gBoard gs))
	(gTanks gs)
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

moveBullet :: Bullet -> GameState -> (Maybe Bullet, GameState)
moveBullet bullet gs =
	-- kill tanks
	let player = bPlayer bullet in
	let newPos = moveByDir (bPosition bullet) 1 (bDirection bullet) in
	let maybeField = maybeGetField (gBoard gs) newPos  in
	let tanks = getTanksByBulletPosition gs newPos in
	let newGameState = updateTanks (List.filter (\tank ->
		sameTeam player (tPlayer tank) ||
		not (tankOverlap newPos tank)
		)) gs
	in
	-- destroy bricks
	let fields = getFieldsByBullet (gBoard gs) (bullet {bPosition = newPos}) in
	let lastGameState = updateFields (mapWithKey
		(\k -> \v -> if (isNothing $ List.find (== (k, v)) fields) || v /= Bricks then v else Empty))
		newGameState
	in
	-- move Bullet
	if List.filter ((/= bPlayer bullet) . tPlayer) (traceShowId tanks) /= []
		|| fields /= []
		|| (isNothing $ maybeGetField (gBoard gs) newPos)
	then (Nothing, lastGameState)
	else (Just $ bullet {bPosition = newPos}, lastGameState)

moveBulletsList :: [Bullet] -> GameState -> ([Bullet], GameState)
moveBulletsList [] gs = ([], gs)
moveBulletsList (x:xs) gs =
	let (y, newgs) = moveBullet x gs in
	let (ys, lastgs) = moveBulletsList xs newgs in
	case y of
		Just b -> (b:ys, lastgs)
		Nothing -> (ys, lastgs)

moveBulletsTank :: Tank -> GameState -> (Tank, GameState)
moveBulletsTank tank gs =
	let (bullets, newgs) = moveBulletsList (tBullets tank) gs in
	(updateBullets tank bullets, newgs)

moveBulletsTanks :: GameState -> [Tank] -> ([Tank], GameState)
moveBulletsTanks gs [] = ([], gs)
moveBulletsTanks gs (x:xs) =
	let (y, newgs) = moveBulletsTank x gs in
	let (ys, lastgs) = moveBulletsTanks newgs xs in
	(y:ys, lastgs)

moveBullets :: GameState -> GameState
moveBullets gs =
	let (tanks, gameState) = moveBulletsTanks gs (gTanks gs)
	in
	updateTanks (\gsTanks -> List.filter (\tank ->
			isJust $ List.find ((== (tPlayer tank)) . tPlayer) gsTanks)
		tanks
	) gameState

updateGameState :: GameState -> IORef MovesMap -> IO GameState
updateGameState gs movesMap = do {
	moves <- readIORef movesMap;
	let (newMoves, newGameState) = modifyMoves (Data.Map.toList moves) gs in
	let finalGameState = moveBullets (moveBullets newGameState) in
	do {
		writeIORef movesMap (Data.Map.fromList newMoves);
		return finalGameState;
	}
}

