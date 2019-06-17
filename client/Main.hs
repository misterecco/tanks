{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
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
import Foreign.C.Types (CInt)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import SDL (($=))
import System.IO as Sys

import Board
import Action
import Drawing


main :: IO ()
main = withSocketsDo $ U.withSDL $ U.withWindow "Tank 1990" (640, 480) $
  \w -> U.withRenderer w $ \r -> do
    SDL.rendererDrawColor r $= SDL.V4 minBound minBound minBound maxBound
    t <- SDL.Image.loadTexture r "./assets/tanks_alpha.png"

    let doRender = \game -> SDL.clear r >> drawGame r t game >> SDL.present r

    addr <- resolve "127.0.0.1" "4242"
    E.bracket (open addr) close (talkSock doRender)

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

    talkSock doRender sock = do
        hdl <- socketToHandle sock ReadWriteMode
        BSL.hPutStrLn hdl (encodeGameAction NewPlayer)
        forkIO $ talk hdl
        readStream hdl doRender

    talk hdl = do
        fix $ \loop -> do
            e <- SDL.eventPayload <$> SDL.waitEvent
            -- print $ "Last event was: " ++ show e
            when (isAction e) $ BSL.hPutStrLn hdl (encodeGameAction (toAction e))
            case e of
                SDL.QuitEvent -> Sys.putStrLn "OK, quit"
                _ -> loop

    readStream hdl doRender = do
        msg <- BS.hGetLine hdl
        Sys.putStr "Received: "
        let game = decodeGameState msg
        Sys.putStrLn $ show (game)
        doRender game
        readStream hdl doRender
