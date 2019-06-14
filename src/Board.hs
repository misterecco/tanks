{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Board where

import Data.Array
import Data.Serialize
import Data.Binary
import Data.Bits
import GHC.Generics (Rep, Generic)

data Field =
      Bricks
    | Forest
    | Stone
    | Ice
    | Empty
    deriving (Generic, Show)

instance Binary Field

type Board = Array (Int, Int) Field

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
