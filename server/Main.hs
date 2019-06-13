module Main where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.IORef
import Data.Map
import Board

data GameAction =
      UP
    | DOWN
    | LEFT
    | RIGHT
    | SHOOT
    deriving Show

-- Change to Move Int GameAction
data GameEvent =
      Move Int String
    | SendBoard Board

type Msg = GameEvent
type MovesMap = Map Int String

readMoves :: Chan Msg -> IORef MovesMap -> IO ()
readMoves chan moves = do
    e <- readChan chan
    case e of
        Move senderNum action -> do {
            modifyIORef moves (insert senderNum action);
            putStrLn $ "Read from " ++ (show senderNum) ++ ": " ++ action
        }
        _ -> return ()
    readMoves chan moves

runServer :: Chan Msg -> IORef MovesMap -> Board -> IO ()
runServer chan moves board = do
	currMoves <- readIORef moves
	writeChan chan (SendBoard board)
	-- wait 1s
	threadDelay 1000000
	runServer chan moves board

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 (tupleToHostAddress (127,0,0,1)))
  listen sock 2
  chan <- newChan
  _ <- forkIO $ fix $ \loop -> do
    _ <- readChan chan
    loop
  map <- newIORef Data.Map.empty
  forkIO (runServer chan map (getBoard 10 10))
  forkIO (readMoves chan map)
  mainLoop sock chan 1

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan msgNum)
  mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan playerNum = do
    let broadcast msg = writeChan chan (Move playerNum msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    hPutStrLn hdl "Welcome to the tanks game, what's your name?"
    name <- fmap init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered the game.")
    hPutStrLn hdl ("Welcome, " ++ name ++ "!")

    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        e <- readChan commLine
        case e of
            SendBoard board -> hPutStrLn hdl (show board)
            _ -> return ()
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


