module AI where

import qualified Data.List as List

import Data.Set as Set
import Data.Map as Map
import Data.Maybe
import Board
import Action

import Debug.Trace

data Goal = GoalPlayer Player | GoalEagle

positionOfGoal :: Goal -> GameState -> Position
positionOfGoal GoalEagle _ = eaglePosition
positionOfGoal (GoalPlayer pl) gs =
  fromMaybe eaglePosition $ fmap tPosition (List.find (\tank -> tPlayer tank == pl) (gTanks gs))

canMove :: GameState -> Set Position -> (Position, GameAction) -> Bool
canMove gs unchecked (pos, action) =
	let fields = getFieldsByTank (gBoard gs) pos in
	let tanks = getTanksByTankPosition gs pos in
	length fields == 4 && all canEnterField fields && Set.member pos unchecked

allMoves :: Position -> GameAction -> [(Position, GameAction)]
allMoves (x, y) action =
  List.map (\a -> (a, action)) [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

startMoves :: Position -> [(Position, GameAction)]
startMoves (x, y) =
  [
    ((x + 1, y), Move UP),
    ((x - 1, y), Move DOWN),
    ((x, y + 1), Move RIGHT),
    ((x, y - 1), Move DOWN)
  ]

insertList :: Ord k => [(k, v)] -> Map k v -> Map k v
insertList list m =
  List.foldr (\(k, v) -> Map.insert k v) m list

dijkstra :: GameState -> Map Position GameAction -> Set Position -> Map Position GameAction
dijkstra gs howToGet unchecked =
  let f (pos, action) = List.filter (canMove gs unchecked) (allMoves pos action) in
  let newGet = List.concat $ List.map f $ Map.toList howToGet in
  if newGet == [] then howToGet else
  let newHow = insertList newGet howToGet in
  let newUnchecked = List.foldr Set.delete unchecked (List.map fst newGet) in
  dijkstra gs newHow newUnchecked

dijkstraInit :: Position -> GameState -> Map Position GameAction
dijkstraInit pos gs =
  let unchecked = Set.fromList $ allFields gs in
  let newGet = List.filter (canMove gs unchecked) (startMoves pos) in
  if newGet == [] then Map.empty else
  let newUnchecked = List.foldr Set.delete unchecked (List.map fst newGet) in
  dijkstra gs (Map.fromList newGet) newUnchecked

positionMove :: Position -> Position -> GameState -> GameAction
positionMove start pos gs =
  let dij = dijkstraInit start gs in
  case Map.lookup pos dij of
    Just x -> x
    Nothing -> trace "Uuuuups" (Move UP)

goalMove :: Goal -> GameState -> Tank -> GameAction
goalMove goal gs tank =
  positionMove (tPosition tank) (positionOfGoal goal gs) gs

smartMove :: GameState -> Tank -> GameAction
smartMove = goalMove GoalEagle

randomMove :: Int -> GameAction
randomMove r =
    case r `mod` 3 of
      0 -> Shoot
      _ -> case r `mod` 5 of
            0 -> Move UP
            1 -> Move DOWN
            2 -> Move LEFT
            3 -> Move RIGHT
            4 -> Shoot

doAIMove :: Int -> GameState -> Tank -> GameAction
doAIMove r gs tank = randomMove r