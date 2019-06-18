module Main where

import Network.Socket
import System.IO as IO
import System.Random
import Control.Exception
import Control.Concurrent
import Control.Monad (when, forM_)
import Control.Monad.State
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
--            IO.putStrLn $ "Read from " ++ (show senderNum) ++ ": " ++ (show action)
        }
        _ -> return ()
    readMoves chan moves

runServer :: Chan Msg -> IORef MovesMap -> GameState -> IO ()
runServer chan moves gameState = do
	currMoves <- readIORef moves;
	IO.putStrLn $ show currMoves;
	if gEagle gameState == Dead
		then runServer chan moves gameState
		else let (_, newState) = runState (updateGameState currMoves) gameState in do
			writeIORef moves Data.Map.empty
			writeChan chan (SendGameState newState)
			--  	IO.putStrLn $ show newState
			-- wait 0.25s
			forkIO (makeNPCMoves chan newState)
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

addNPCs :: Chan Msg -> Int -> IO ()
addNPCs chan i = do
  r <- abs <$> randomIO
  writeChan chan (Action (NPC i) (NewPlayer r))
  threadDelay 1000000
  addNPCs chan (i + 1)

makeNPCMoves :: Chan Msg -> GameState -> IO ()
makeNPCMoves chan gs = let
  npcs = Prelude.filter isNPC (gTanks gs)
  isNPC tank = case tPlayer tank of
    NPC _ -> True
    Human _ -> False
  makeMoves npc = do
    let pl = tPlayer npc
    r <- abs <$> (randomIO :: IO Int)
    case r `mod` 3 of
      0 -> writeChan chan (Action pl (Shoot))
      _ -> return ()
    case r `mod` 5 of
      0 -> writeChan chan (Action pl (Move UP))
      1 -> writeChan chan (Action pl (Move DOWN))
      2 -> writeChan chan (Action pl (Move LEFT))
      3 -> writeChan chan (Action pl (Move RIGHT))
      _ -> return ()
    in
  forM_ npcs makeMoves


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
  map <- newIORef Data.Map.empty
  forkIO (runServer chan map (initialGameState firstLevel))
  forkIO (readMoves chan map)
  forkIO (addNPCs chan 0)
  mainLoop sock chan 0

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

    writeChan chan (Action (Human playerNum) (NewPlayer 0))

    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        e <- readChan commLine
        case e of
            SendGameState gameState -> do {
				BSL.hPutStrLn hdl (encodeGameState gameState)
				}
            _ -> return ()
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- BS.hGetLine hdl
        broadcast line
        loop

    killThread reader                      -- kill after the loop ends
    hClose hdl                             -- close the handle


