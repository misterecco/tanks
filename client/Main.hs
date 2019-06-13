{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Main where

import qualified Control.Exception as E
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO
import Control.Concurrent
import Graphics.Vty
import Graphics.Vty.Config
import Control.Monad
import Control.Monad.Fix

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
        hPutStrLn hdl "Player 1"
        forkIO (readStream hdl)
        talk hdl
    	
    talk hdl = do
        config <- standardIOConfig
        vty <- mkVty config
        fix $ \loop -> do
            e <- nextEvent vty
            print $ "Last event was: " ++ show e
            case e of
                EvKey KLeft _ -> hPutStrLn hdl "LEFT" >> loop
                EvKey KRight _ -> hPutStrLn hdl "RIGHT" >> loop
                EvKey KUp _ -> hPutStrLn hdl "UP" >> loop
                EvKey KDown _ -> hPutStrLn hdl "DOWN" >> loop
                EvKey (KChar 'c') [MCtrl] -> Graphics.Vty.shutdown vty >> putStrLn "OK, quit"
                _ -> loop
      		 
    readStream hdl = do
		msg <- hGetLine hdl
		putStr "Received: "
		putStrLn msg
		readStream hdl
