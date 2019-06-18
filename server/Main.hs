module Main where

import qualified Control.Concurrent.Chan.Synchronous as Sync

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
import Data.List
import Data.ByteString.Lazy.Char8 as BSL
import Data.ByteString as BS
import Data.Binary
import Board
import Action
import AI

import Debug.Trace

-- Change to Move Int GameAction
data GameEvent =
      Action Player GameAction
    | SendGameState GameState

type Msg = GameEvent

data QueueEvent =
    PlayerConnected Player
  | PlayerDisconnected Player
  deriving (Show)

readMoves :: Chan Msg -> IORef MovesMap -> IO ()
readMoves chan moves = do
    e <- readChan chan
    case e of
        Action senderNum action -> do {
            modifyIORef moves (updateMovesMap senderNum action);
            if isHuman senderNum
            then IO.putStrLn $ "Read from " ++ (show senderNum) ++ ": " ++ (show action)
            else return ()
        }
        _ -> return ()
    readMoves chan moves

eventToMove :: QueueEvent -> (Player, GameAction)
eventToMove (PlayerConnected pl) = (pl, GiveLives 3)
eventToMove (PlayerDisconnected pl) = (pl, DestroyPlayer)

readAllAvailable :: Sync.Chan QueueEvent -> IO [QueueEvent]
readAllAvailable eventsChan = do
  res <- Sync.maybeTry $ Sync.tryReadChan eventsChan
  case res of
    Nothing -> return []
    Just y -> do ys <- readAllAvailable eventsChan
                 return (y:ys)

runServer :: Chan Msg -> Sync.Chan QueueEvent -> IORef MovesMap -> GameState -> IO ()
runServer chan eventsChan moves gameState = do
	events <- readAllAvailable eventsChan;
	prevMoves <- readIORef moves;
	writeIORef moves Data.Map.empty
	let currMoves = Data.List.map eventToMove (traceShowId events) ++ Data.Map.toList prevMoves in do
	  IO.putStrLn $ show currMoves;
	  if gEagle gameState == Dead
		  then runServer chan eventsChan moves gameState
		  else let (_, newState) = runState (updateGameState currMoves) gameState in do
			  writeChan chan (SendGameState newState)
			  --  	IO.putStrLn $ show newState
			  -- wait 0.25s
			  forkIO (makeNPCMoves chan newState)
			  threadDelay 250000
			  runServer chan eventsChan moves newState



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
  threadDelay 250000
  addNPCs chan (i + 1)

makeNPCMoves :: Chan Msg -> GameState -> IO ()
makeNPCMoves chan gs = let
  npcs = Prelude.filter isNPC (gTanks gs)
  isNPC tank = case tPlayer tank of
    NPC _ -> True
    Human _ -> False
  makeMoves gs npc = do
    r <- abs <$> (randomIO :: IO Int)
    let action = doAIMove r gs npc in writeChan chan (Action (tPlayer npc) action)
  in
  forM_ npcs (makeMoves gs)

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
  eventChan <- Sync.newChan
  map <- newIORef Data.Map.empty
  forkIO (runServer chan eventChan map (initialGameState firstLevel))
  forkIO (readMoves chan map)
  forkIO (addNPCs chan 0)
  mainLoop sock chan eventChan 0

mainLoop :: Socket -> Chan Msg -> Sync.Chan QueueEvent -> Int -> IO ()
mainLoop sock chan eventsChan msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan eventsChan msgNum)
  mainLoop sock chan eventsChan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Sync.Chan QueueEvent -> Int -> IO ()
runConn (sock, _) chan eventsChan playerNum = do

    let broadcast msg = writeChan chan (Action (Human playerNum) (decodeGameAction msg))
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    Sync.writeChan eventsChan (PlayerConnected (Human playerNum))

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

    Sync.writeChan eventsChan (PlayerDisconnected (Human playerNum))

    killThread reader                      -- kill after the loop ends
    hClose hdl                             -- close the handle


