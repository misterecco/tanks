{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BlockArguments #-}

module Action where

import qualified SDL
import qualified Data.List as List

import Data.Aeson

import Data.Maybe
import Data.Map
import Data.IORef
import Control.Monad.State
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

newTankPos :: Player -> Tank
newTankPos (Human pid) = newPlayerTank pid
-- TODO: randomness, make sure the field is available
newTankPos (NPC pid) = newNPCTank pid 0 0

addNewTank :: Player -> GameStateM ()
addNewTank pl = do
  gs <- get
  put $ gs {
      gTanks = (newTankPos pl : (gTanks gs))
    }

moveField :: GameState -> Dir -> Position -> Position
moveField gs dir pos =
	let newPos = moveByDir pos 1 dir in
	let fields = getFieldsByTank (gBoard gs) newPos in
	let tanks = getTanksByTankPosition gs newPos in
	if length fields == 4 && length tanks == 1 && all canEnterField fields then newPos else pos

moveFieldTank :: GameState -> Tank -> Tank
moveFieldTank gs tank = tank { tPosition =
		moveField gs (tDirection tank) (tPosition tank)
	}

moveTank :: GameState -> Player -> Dir -> [Tank] -> [Tank]
moveTank _ _ _ [] = []
moveTank gs pl dir (tank:xs) =
	if tPlayer tank == pl
	then moveFieldTank gs (tank { tDirection = dir }) : xs
	else tank : moveTank gs pl dir xs

bulletVelocity ::Velocity
bulletVelocity = (4, 0)

shootTank :: Player -> [Tank] -> [Tank]
shootTank _ [] = []
shootTank pl (tank:xs) =
	if tPlayer tank == pl
	then newTank:xs
	else tank:(shootTank pl xs)
	where newTank = tank { tBullets =
		(Bullet
			(tDirection tank)
			(barrelPosition tank)
			bulletVelocity
			(tPlayer tank)
		):(tBullets tank)
		}

updateBullets :: Tank -> [Bullet] -> Tank
updateBullets tank bullets = tank { tBullets = bullets }

updateTanks :: ([Tank] -> [Tank]) -> GameStateM ()
updateTanks f = do
    gs <- get;
    put $ gs { gTanks = f $ gTanks gs }

updateFieldsBoard :: (Map Position Field -> Map Position Field) -> Board -> Board
updateFieldsBoard f (Board n m b) = Board n m (f b)

updateFields :: (Map Position Field -> Map Position Field) -> GameStateM ()
updateFields f = do
    gs <- get
    put $ gs { gBoard = updateFieldsBoard f (gBoard gs) }

modifyMove :: Player -> GameAction -> GameStateM ()
modifyMove p NewPlayer = addNewTank p
modifyMove p (Move d) = do
  gs <- get
  updateTanks $ moveTank gs p d
modifyMove p NoAction = return ()
modifyMove p Shoot = updateTanks (shootTank p)

modifyMoves :: [(Player, GameAction)] -> GameStateM ()
modifyMoves [] = return ()
modifyMoves ((p, action):xs) = do
  modifyMove p action
  modifyMoves xs

moveBullet :: Bullet -> GameStateM (Maybe Bullet)
moveBullet bullet =
	-- kill tanks
	let player = bPlayer bullet in
	let newPos = moveByDir (bPosition bullet) 1 (bDirection bullet) in do {
	tanks <- getTanksByBulletPosition newPos;
	updateTanks $ List.filter (\tank ->
		sameTeam player (tPlayer tank) ||
		not (tankOverlap newPos tank)
		);
	-- destroy bricks
	gs <- get;
	let fields = getFieldsByBullet (gBoard gs) (bullet {bPosition = newPos}) in
	updateFields $ mapWithKey
		(\k -> \v -> if (isNothing $ List.find (== (k, v)) fields) || v /= Bricks then v else Empty);
	-- TODO destroy Bullets
	-- TODO destroy eagle
	-- move Bullet
	let fields = getFieldsByBullet (gBoard gs) (bullet {bPosition = newPos}) in
	if List.filter ((/= bPlayer bullet) . tPlayer) (traceShowId tanks) /= []
		|| fields /= []
		|| (isNothing $ maybeGetField (gBoard gs) newPos)
	then return Nothing
	else return $ Just $ bullet {bPosition = newPos}
}

moveBulletsList :: [Bullet] -> GameStateM [Bullet]
moveBulletsList [] = return []
moveBulletsList (x:xs) = do
	y <- moveBullet x
	ys <- moveBulletsList xs
	case y of
		Just b -> return $ b:ys
		Nothing -> return ys

moveBulletsTank :: Tank -> GameStateM Tank
moveBulletsTank tank = do
	bullets <- moveBulletsList (tBullets tank)
	return $ updateBullets tank bullets

moveBulletsTanks :: [Tank] -> GameStateM [Tank]
moveBulletsTanks [] = return []
moveBulletsTanks (x:xs) = do
	y <- moveBulletsTank x
	ys <- moveBulletsTanks xs
	return $ y:ys

moveBullets :: GameStateM ()
moveBullets = do
  gs <- get
  tanks <- moveBulletsTanks (gTanks gs)
  updateTanks (\gsTanks -> List.filter (\tank ->
			isJust $ List.find ((== (tPlayer tank)) . tPlayer) gsTanks)
		tanks
	)

updateGameState :: MovesMap -> GameStateM ()
updateGameState moves = do
	modifyMoves (Data.Map.toList moves)
	moveBullets
	moveBullets

