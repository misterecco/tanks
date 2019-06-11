module Bindings (idle, display, reshape, keyboardMouse) where

import Data.IORef
import Graphics.UI.GLUT

import Display

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)

keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse angle pos key Down _modifiers _position = case key of
  (Char ' ') -> angle $~! negate
  (Char '+') -> angle $~! (* 2)
  (Char '-') -> angle $~! (/ 2)
  (SpecialKey KeyLeft ) -> pos $~! \(x,y) -> (x-0.1,y)
  (SpecialKey KeyRight) -> pos $~! \(x,y) -> (x+0.1,y)
  (SpecialKey KeyUp   ) -> pos $~! \(x,y) -> (x,y+0.1)
  (SpecialKey KeyDown ) -> pos $~! \(x,y) -> (x,y-0.1)
  _ -> return ()
keyboardMouse _ _ _ _ _ _ = return ()
