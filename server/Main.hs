module Main where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.IORef
import Data.Map

type Msg = (Int, String)

readMoves :: Chan Msg -> IORef (Map Int String) -> IO ()
readMoves chan moves = do
	(senderNum, line) <- readChan chan
	when (senderNum /= 0) $ do {
		modifyIORef moves (insert senderNum line);
		putStrLn $ "Read from " ++ (show senderNum) ++ ": " ++ line
	}
	readMoves chan moves

runServer :: Chan Msg -> IORef (Map Int String) -> IO ()
runServer chan moves = do
	currMoves <- readIORef moves
	writeChan chan (0, "This is a board " ++ (show currMoves))
	-- wait 1s
	threadDelay 1000000
	runServer chan moves

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 (tupleToHostAddress (127,0,0,1)))
  listen sock 2
  chan <- newChan
  _ <- forkIO $ fix $ \loop -> do
    (_, _) <- readChan chan
    loop
  map <- newIORef Data.Map.empty
  forkIO (runServer chan map)
  forkIO (readMoves chan map)
  mainLoop sock chan 1

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan msgNum)
  mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan playerNum = do
    let broadcast msg = writeChan chan (playerNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    hPutStrLn hdl "Welcome to the tanks game, what's your name?"
    name <- fmap init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered the game.")
    hPutStrLn hdl ("Welcome, " ++ name ++ "!")

    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (senderNum, line) <- readChan commLine
        when (senderNum == 0) $ hPutStrLn hdl line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- hGetLine hdl
        case line of
             -- If an exception is caught, send a message and break the loop
             "quit" -> hPutStrLn hdl "Bye!"
             -- else, continue looping.
             _      -> broadcast line >> loop

    killThread reader                      -- kill after the loop ends
    broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
    hClose hdl                             -- close the handle


