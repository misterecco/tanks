{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Board where

import Data.Array
import Data.Binary
import Data.Bits
import Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy (ByteString, fromStrict)
import GHC.Generics (Rep, Generic)

data Field =
      Bricks
    | Forest
    | Stone
    | Ice
    | Empty
    deriving (Generic, Show)

instance Binary Field

data Dir =
	  UP
	| DOWN
	| LEFT
	| RIGHT
	deriving (Generic, Show)

instance Binary Dir

type Bullet = (Dir, Int, Int)

data Tank = Tank (Int, Int) Dir [Bullet]
	deriving (Generic, Show)

instance Binary Tank

type Board = Array (Int, Int) Field

data GameState = GameState Board [Tank]
	deriving (Generic, Show)
	
instance Binary GameState

x_coeff :: Int
x_coeff = 17

y_coeff :: Int
y_coeff = 67

randomField :: Int -> Int -> Field
randomField i j =
    case ((i * 1000 + j) `xor` 8475845) `mod` 5 of
        0 -> Bricks
        1 -> Forest
        2 -> Stone
        3 -> Ice
        _ -> Empty


getBoard :: Int -> Int -> Board
getBoard n m = array ((0,0),(n-1,m-1)) (concat [ [ ((i, j), Empty) | j <- [0..n-1] ] | i <- [0..m-1]] )

randomBoard :: Int -> Int -> Board
randomBoard n m = array ((0,0),(n-1,m-1)) (concat [ [ ((i, j), randomField i j) | j <- [0..n-1] ] | i <- [0..m-1]] )

decodeBoard :: BS.ByteString -> Board
decodeBoard str = decode $ fromStrict str

encodeGameState :: GameState -> Data.ByteString.Lazy.ByteString
encodeGameState gs =  encode gs

decodeGameState :: BS.ByteString -> GameState
decodeGameState str = decode $ fromStrict str
