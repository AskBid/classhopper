{-# LANGUAGE OverloadedStrings #-}

module Interface where

import Graphics.UI.GLFW
import Control.Monad (unless)
import qualified Graphics.Rendering.OpenGL as GL
import Prelude hiding (init)

import qualified Drawing as D

launchWindow :: IO ()
launchWindow = do
  -- Initialize GLFW
  initSuccess <- init
  if not initSuccess
    then error "Failed to initialize GLFW"
    else do
      window <- createWindow 800 600 "Classhopper 3D" Nothing Nothing
      case window of
        Nothing -> error "Failed to create GLFW window"
        Just win -> do
          makeContextCurrent (Just win)
          swapInterval 1
          -- _ <- initVAO
          appLoop win
          destroyWindow win
          terminate

appLoop :: Window -> IO ()
appLoop window = do
  shouldClose <- windowShouldClose window
  unless shouldClose $ do
    pollEvents
    GL.clearColor GL.$= GL.Color4 0.43 0.43 0.47 1
    GL.clear [GL.ColorBuffer]
    D.drawing
    swapBuffers window
    appLoop window

