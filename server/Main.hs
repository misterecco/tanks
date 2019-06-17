module Main where

import Network.Socket
import System.IO as IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.IORef
import Data.Map
import Data.ByteString.Lazy.Char8 as BSL
import Data.ByteString as BS
import Data.Binary
import Board
import Action

-- Change to Move Int GameAction
data GameEvent =
      Action Player GameAction
    | SendGameState GameState

type Msg = GameEvent

readMoves :: Chan Msg -> IORef MovesMap -> IO ()
readMoves chan moves = do
    e <- readChan chan
    case e of
        Action senderNum action -> do {
            modifyIORef moves (updateMovesMap senderNum action);
            IO.putStrLn $ "Read from " ++ (show senderNum) ++ ": " ++ (show action)
        }
        _ -> return ()
    readMoves chan moves

runServer :: Chan Msg -> IORef MovesMap -> GameState -> IO ()
runServer chan moves gameState = do
	currMoves <- readIORef moves
	newState <- updateGameState gameState moves
	writeChan chan (SendGameState newState)
	IO.putStrLn $ show newState
	IO.putStrLn $ show currMoves
	-- wait 0.25s
	threadDelay 250000
	runServer chan moves newState

resolve :: String -> IO AddrInfo
resolve port = do
	let hints = defaultHints {
			addrFlags = [AI_PASSIVE]
		  , addrSocketType = Stream
		  }
	addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
	return addr

main :: IO ()
main = do
  addr <- resolve "4242"
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  -- If the prefork technique is not used,
  -- set CloseOnExec for the security reasons.
  let fd = fdSocket sock in setCloseOnExecIfNeeded fd
  bind sock (addrAddress addr)
  listen sock 10
  chan <- newChan
  _ <- forkIO $ fix $ \loop -> do
    _ <- readChan chan
    loop
  map <- newIORef Data.Map.empty
  forkIO (runServer chan map (initialGameState firstLevel))
  forkIO (readMoves chan map)
  mainLoop sock chan 1

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan msgNum)
  mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan playerNum = do
    let broadcast msg = writeChan chan (Action (Human playerNum) (decodeGameAction msg))
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        e <- readChan commLine
        case e of
            SendGameState gameState -> do {
				BSL.hPutStrLn hdl ((encodeGameState gameState))
				}
            _ -> return ()
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- BS.hGetLine hdl
        broadcast line
        loop

    killThread reader                      -- kill after the loop ends
    hClose hdl                             -- close the handle


