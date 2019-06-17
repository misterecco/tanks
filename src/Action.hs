{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Action where

import Data.Binary
import Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy (ByteString, fromStrict)
import GHC.Generics (Generic)
import Graphics.Vty
import Board

data GameAction =
      Move Dir
    | SHOOT
    | InvalidAction
  deriving (Binary, Generic, Show, Eq)

toAction :: Event -> GameAction
toAction (EvKey KLeft _) = Move LEFT
toAction (EvKey KRight _) = Move RIGHT
toAction (EvKey KUp _) = Move UP
toAction (EvKey KDown _) = Move DOWN
toAction _ = InvalidAction


isAction :: Event -> Bool
isAction e = toAction e /= InvalidAction

encodeGameAction :: GameAction -> Data.ByteString.Lazy.ByteString
encodeGameAction gs =  encode gs

decodeGameAction :: BS.ByteString -> GameAction
decodeGameAction str = decode $ fromStrict str
