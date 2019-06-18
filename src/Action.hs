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
    | NewPlayer Int
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
	f (Just (NewPlayer r)) = Just (NewPlayer r)
	f (Just _) = Just gameAction

addNewTank :: Player -> Int -> GameStateM ()
addNewTank pl r = case pl of
  (Human pid) -> do
    gs <- get;
    put $ gs { gTanks = ((newPlayerTank pid) : (gTanks gs)) }
  (NPC pid) -> do
    gs <- get;
    case findNPCTankSlot pid gs of
      Nothing -> return ()
      Just x -> put $ gs { gTanks = ((newNPCTank pid x r) : (gTanks gs)) }

findNPCTankSlot :: Int -> GameState -> Maybe Int
findNPCTankSlot i gs = let
  tanks = gTanks gs
  slots = case i `mod` 3 of
    0 -> [0, 12, 24]
    1 -> [12, 24, 0]
    2 -> [24, 0, 12]
  checkTank slot tank = let
    (x, y) = tPosition tank
    in (y == 0 || y == 1) && (x == slot || x == slot + 1)
  in
    if (length tanks) > 5
    then Nothing
    else List.find (\slot -> not $ List.any (checkTank slot) tanks) slots

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
	then if tDirection tank == dir
	  then moveFieldTank gs tank : xs
	  else tank { tDirection = dir } : xs
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

updateDestroyBulletsBullet :: Bullet -> Bullet -> Bullet -> Maybe Bullet
updateDestroyBulletsBullet oldBullet bullet b =
    let oldPos = bulletPositions oldBullet in
    let newPos = bulletPositions bullet in
    let posEnemy = bulletPositions b in
    let l1 = List.intersect oldPos posEnemy in
    let l2 = List.intersect newPos posEnemy in
    if (l1 /= [] && l2 /= []) || (List.length l2 == 2) then Nothing else Just b

updateDestroyBulletsBullets :: Bullet -> Bullet -> [Bullet] -> [Bullet]
updateDestroyBulletsBullets a b xs = Data.Maybe.mapMaybe (updateDestroyBulletsBullet a b) xs

updateDestroyBulletsTank :: Bullet -> Bullet -> Tank -> GameStateM Tank
updateDestroyBulletsTank oldBullet bullet tank =
  if tPlayer tank == bPlayer bullet
  then return tank
  else return $ updateBullets tank (updateDestroyBulletsBullets oldBullet bullet (tBullets tank))

updateDestroyBulletsTanks :: Bullet -> Bullet -> [Tank] -> GameStateM [Tank]
updateDestroyBulletsTanks _ _ [] = return []
updateDestroyBulletsTanks oldBullet bullet (x:xs) = do
  y <- updateDestroyBulletsTank oldBullet bullet x
  ys <- updateDestroyBulletsTanks oldBullet bullet xs
  return (y:ys)

updateDestroyBullets :: Bullet -> Bullet -> GameStateM Bool
updateDestroyBullets oldBullet bullet = do
  gs <- get;
  tanks <- updateDestroyBulletsTanks oldBullet bullet (gTanks gs)
  if gTanks gs == tanks then return False else do
    put $ gs { gTanks = tanks }
    return True

checkEagleBullet :: Bullet -> GameStateM ()
checkEagleBullet bullet =
  let positions = bulletPositions bullet in
  if not (isHuman $ bPlayer bullet) && (isJust $ List.find (== eaglePosition) positions)
  then do
    gs <- get
    put $ gs { gEagle = Dead }
  else return ()

modifyMove :: Player -> GameAction -> GameStateM ()
modifyMove p (NewPlayer r) = addNewTank p r
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
	updateTanks $ List.map (\tank ->
		if sameTeam player (tPlayer tank) || not (tankOverlap newPos tank)
		then tank
		else case traceShowId $ nextColor (tColor tank) of
		  Nothing -> tank { tStatus = Destroyed }
		  Just col -> tank { tColor = col }
		);
	-- destroy bricks
	gs <- get;
	let fields = getFieldsByBullet (gBoard gs) (bullet {bPosition = newPos}) in
	updateFields $ mapWithKey
		(\k -> \v -> if (isNothing $ List.find (== (k, v)) fields) || v /= Bricks then v else Empty);
	-- destroy Bullets
	destroyed <- updateDestroyBullets bullet $ bullet {bPosition = newPos};
	-- destroy eagle
	checkEagleBullet $ bullet {bPosition = newPos};
	-- move Bullet
	let fields = getFieldsByBullet (gBoard gs) (bullet {bPosition = newPos}) in
	if
	  destroyed
	  || List.filter ((/= bPlayer bullet) . tPlayer) (traceShowId tanks) /= []
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

moveBulletsTanks :: [Tank] -> GameStateM ()
moveBulletsTanks [] = return ()
moveBulletsTanks (x:xs) = do
  gs <- get
  case List.find ((== tPlayer x) . tPlayer) (gTanks gs) of
    Nothing -> return ()
    Just oldTank -> do
      tank <- moveBulletsTank oldTank
      updateTanks $ List.map (\gsTank -> if tPlayer gsTank == tPlayer tank then tank else gsTank)
  moveBulletsTanks xs

moveBullets :: GameStateM ()
moveBullets = do
  gs <- get
  moveBulletsTanks (gTanks gs)

filterDestroyed :: GameStateM ()
filterDestroyed = do
  gs <- get
  updateTanks (List.filter (\t -> tStatus t == Working))

updateGameState :: MovesMap -> GameStateM ()
updateGameState moves = do
  filterDestroyed
  modifyMoves (Data.Map.toList moves)
  moveBullets
  moveBullets

