module Main where

import Network.Socket
import System.IO as IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.IORef
import Data.Map
import Data.ByteString.Lazy.Char8 as BS
import Data.Binary
import Board

data GameAction =
      Move Dir
    | SHOOT
    deriving Show

-- Change to Move Int GameAction
data GameEvent =
      Action Int String
    | SendGameState GameState

type Msg = GameEvent
type MovesMap = Map Int String

readMoves :: Chan Msg -> IORef MovesMap -> IO ()
readMoves chan moves = do
    e <- readChan chan
    case e of
        Action senderNum action -> do {
            modifyIORef moves (insert senderNum action);
            IO.putStrLn $ "Read from " ++ (show senderNum) ++ ": " ++ action
        }
        _ -> return ()
    readMoves chan moves

runServer :: Chan Msg -> IORef MovesMap -> Board -> IO ()
runServer chan moves board = do
	currMoves <- readIORef moves
	writeChan chan (SendGameState (GameState board [Tank (0, 0) UP []]))
	IO.putStrLn $ show (GameState board [Tank (0, 0) UP []])
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
  forkIO (runServer chan map (getBoard 5 5))
  forkIO (readMoves chan map)
  mainLoop sock chan 1

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan msgNum)
  mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan playerNum = do
    let broadcast msg = writeChan chan (Action playerNum msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    name <- IO.hGetLine hdl
    broadcast ("--> " ++ name ++ " entered the game.")

    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        e <- readChan commLine
        case e of
            SendGameState gameState -> BS.hPutStrLn hdl ((encodeGameState gameState))
            _ -> return ()
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- IO.hGetLine hdl
        case line of
             -- If an exception is caught, send a message and break the loop
             "quit" -> IO.hPutStrLn hdl "Bye!"
             -- else, continue looping.
             _      -> broadcast line >> loop

    killThread reader                      -- kill after the loop ends
    broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
    hClose hdl                             -- close the handle


