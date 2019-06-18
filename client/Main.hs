{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception as E
import qualified SDL
import qualified SDL.Image
import qualified SDLUtils as U

import Control.Concurrent
import Control.Monad
import Control.Monad.Extra (whileM)
import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString as BS
import Data.ByteString.Lazy.Char8 as BSL
import Data.Binary
import Data.IORef
import Foreign.C.Types (CInt)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import SDL (($=))
import System.Exit
import System.Environment
import System.IO as Sys

import Board
import Action
import Drawing


main :: IO ()
main = withSocketsDo $ U.withSDL $ U.withWindow "Tank 1990" (1280, 800) $
  \w -> U.withRenderer w $ \r -> do

    args <- getArgs
    when (Prelude.length args == 0) $ do
        print "Usage: client-exe serverAddress"
        exitFailure

    SDL.rendererDrawColor r $= SDL.V4 minBound minBound minBound maxBound
    t <- SDL.Image.loadTexture r "./assets/tanks_alpha.png"
    SDL.textureBlendMode t $= SDL.BlendAlphaBlend

    let doRender = \game -> SDL.clear r >> drawGame r t game >> SDL.present r
    shouldExit <- newIORef False

    addr <- resolve (Prelude.head args) "4242"
    E.bracket (open addr) close (talkSock doRender shouldExit )

    SDL.destroyTexture t

  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr

    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock

    talkSock doRender shouldExit sock = do
        hdl <- socketToHandle sock ReadWriteMode
        forkIO $ talk hdl shouldExit
        readStream hdl doRender shouldExit

    talk hdl shouldExit = do
        fix $ \loop -> do
            e <- SDL.eventPayload <$> SDL.waitEvent
            -- print $ "Last event was: " ++ show e
            when (isAction e) $ do {
				BSL.hPutStrLn hdl (encodeGameAction (toAction e));
			}
            case e of
                SDL.QuitEvent -> do
                    Sys.putStrLn "OK, quit"
                    writeIORef shouldExit True
                _ -> loop

    readStream hdl doRender shouldExit = do
        msg <- BS.hGetLine hdl
        Sys.putStr "Received: "
        let game = decodeGameState msg
 --       Sys.putStrLn $ show (game)
        Sys.putStrLn $ show $ gPoints game
        doRender game
        se <- readIORef shouldExit
        unless se $ readStream hdl doRender shouldExit
