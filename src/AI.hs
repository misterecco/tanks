module AI where

import Board
import Action

doAIMove :: Int -> GameState -> GameAction
doAIMove r _ =
    case r `mod` 3 of
      0 -> Shoot
      _ -> case r `mod` 5 of
            0 -> Move UP
            1 -> Move DOWN
            2 -> Move LEFT
            3 -> Move RIGHT
            4 -> Shoot