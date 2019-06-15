{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Main where

import qualified Control.Exception as E
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO as Sys
import Control.Concurrent
import Graphics.Vty
import Graphics.Vty.Config
import Control.Monad
import Control.Monad.Fix
import Data.ByteString as BS
import Data.ByteString.Lazy
import Data.Binary
import Board

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve "127.0.0.1" "4242"
    E.bracket (open addr) close talkSock
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock
    talkSock sock = do
        hdl <- socketToHandle sock ReadWriteMode
        Sys.hPutStrLn hdl "Player 1"
        forkIO (readStream hdl)
        talk hdl
    	
    talk hdl = do
        config <- standardIOConfig
        vty <- mkVty config
        fix $ \loop -> do
            e <- nextEvent vty
            print $ "Last event was: " ++ show e
            case e of
                EvKey KLeft _ -> Sys.hPutStrLn hdl "LEFT" >> loop
                EvKey KRight _ -> Sys.hPutStrLn hdl "RIGHT" >> loop
                EvKey KUp _ -> Sys.hPutStrLn hdl "UP" >> loop
                EvKey KDown _ -> Sys.hPutStrLn hdl "DOWN" >> loop
                EvKey (KChar 'c') [MCtrl] -> Graphics.Vty.shutdown vty >> Sys.putStrLn "OK, quit"
                _ -> loop
      		 
    readStream hdl = do
		msg <- BS.hGetLine hdl
		Sys.putStr "Received: "
		Sys.putStrLn $ show (decodeGameState msg)
		readStream hdl
