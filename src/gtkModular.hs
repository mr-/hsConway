{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.Maybe (fromJust, isJust)

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Selectors.FileChooser
import Graphics.UI.Gtk
import Conway 
import Data.Array
import Data.List (unlines, lines, intersperse)
import Control.Monad (forM_, when, unless, liftM)
import Data.IORef
import Control.Concurrent as CC



type Universe = [Grid]

cellSize = 6
defaultSpeed = 300
main :: IO ()
main= do
    initGUI
    window <- windowNew
    table   <- tableNew 20 10 True
    startButton <- buttonNewWithLabel "Start"
    pauseButton <- buttonNewWithLabel "Pause"

    set window [windowTitle := "hsConway",
                windowDefaultWidth := (300), windowDefaultHeight := (500 ),
                containerBorderWidth := 10 ]

    containerAdd window table

    canvas <- drawingAreaNew

    tableAttachDefaults table startButton 0 1 1 2
    tableAttachDefaults table pauseButton 1 2 1 2


    tableAttachDefaults table canvas 3 10 0 20

    widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

    widgetShowAll window 

    drawin <- widgetGetDrawWindow canvas
    curGridRef <- newIORef [glider]

    onClicked startButton ( startButtonHandler  curGridRef drawin pauseButton  )


    onExpose canvas       ( canvasExposeHandler curGridRef drawin )
    
    onDestroy window mainQuit
    mainGUI



canvasExposeHandler curGridRef drawin x = 
  do  curGrid <- readState curGridRef
      let (width, height) = dim curGrid
      drawWindowClear drawin
      renderWithDrawable drawin (drawField width height)                              
      renderWithDrawable drawin (drawGrid curGrid)
      return True




pauseButtonHandler timerID = timeoutRemove timerID


startButtonHandler curGridRef drawin button  =
  do timerID <- timeoutAdd(   doDrawNextStep curGridRef drawin 
                            >>    return True) (defaultSpeed)
     onClicked button ( pauseButtonHandler  timerID )
     return ()


    
doDrawNextStep curGridRef drawin = 
  do curGrid <- readState curGridRef
     let newGrid = nextGen curGrid 
     writeState curGridRef newGrid
     renderWithDrawable drawin (drawDelta (deltaGrid curGrid newGrid))


readState curGridRef = readIORef curGridRef >>= return.head 

readAllState curGridRef = readIORef curGridRef

writeState curGridRef newState = 
  do oldState <- readIORef curGridRef
     writeIORef curGridRef (newState:oldState)

writeAllState curGridRef newState =
  writeIORef curGridRef newState


drawDelta ::   GridDelta -> Render ()
drawDelta  gridDelta = 
    forM_ (assocs gridDelta) (\((x,y), cell) -> case cell of 
                                        Spawn -> drawCell  x y
                                        Kill  -> killCell  x y
                                        _  -> return () )
    >> fill



centerCoords :: Int -> Int -> (Double, Double)
centerCoords a b = ( fromIntegral nx,  fromIntegral ny )
        where nx = cellSize * a - div cellSize 2
              ny = cellSize * b - div cellSize 2


drawCell x y = do
    let (x',y') = centerCoords  x y 
    rectangle (( x')-2) (( y')-2) 4 4
    setSourceRGBA 1 0 0 0.8
    fill


killCell  x y = do
    let (x',y') = centerCoords  x y 
    rectangle (( x')-2) (( y')-2) 4 4
    setSourceRGBA 1 1 1 1
    fill

drawGrid :: Grid -> Render ()
drawGrid grid = 
    forM_ (assocs grid) (\((x,y), cell) -> if cell == Alive then drawCell  x y else return ()) 
    >> fill

drawField :: Int -> Int -> Render ()
drawField w h = do
    setSourceRGB 0 0 0
    setLineWidth 0.5
    moveTo 0 0
    forM_ [1..h] $ \x -> do  moveTo 0 (fromIntegral $ x*cellSize)
                             lineTo   (fromIntegral $ w*cellSize) (fromIntegral $ x*cellSize)
    forM_ [1..w] $ \x -> do  moveTo   (fromIntegral $ x*cellSize) 0
                             lineTo   (fromIntegral $ x*cellSize) (fromIntegral $ h*cellSize)
    stroke







glider = stringToGrid
   ["#...................................#",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    "..................###................",
    "..................#.#................",
    "...................#.................",
    "...................#.................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    ".....................................",
    "..................................###",
    "..................................#..",
    "...................................#.",
    ".....................................",
    "#...................................#"]
