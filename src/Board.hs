#!/usr/bin/env stack
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Board where

import Data.Array
import Data.Serialize
import GHC.Generics (Generic)

data Field =
      Bricks
    | Forest
    | Stone
    | Ice
    | Empty
    deriving (Generic, Show)
    
instance Serialize Field

type Board = Array (Int, Int) Field

getBoard :: Int -> Int -> Board
getBoard n m = array ((0,0),(n-1,m-1)) (concat [ [ ((i, j), Empty) | j <- [0..n-1] ] | i <- [0..m-1]] )
