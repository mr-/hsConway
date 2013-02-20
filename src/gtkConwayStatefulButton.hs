{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.Maybe (fromJust, isJust)

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Selectors.FileChooser
import Graphics.UI.Gtk

import Data.Array
import Data.List
import Control.Monad.State
import Data.IORef
import Control.Concurrent as CC



data Cell = Dead | Alive deriving (Eq, Show)
type Grid = Array (Int,Int) Cell

data Delta = Kill | Spawn | Keep deriving (Eq, Show)
type GridDelta = Array (Int,Int) Delta

data Universe = Universe {
  uBounds :: (Int,Int),
  uStates :: [Grid]
}


cellSize = 15

main :: IO ()
main= do
    initGUI
    window <- windowNew
    table   <- tableNew 20 20 True
    startButton <- buttonNewWithLabel "Start"
    pauseButton <- buttonNewWithLabel "Pause"
    loadButton  <- buttonNewWithLabel "Load"
    set window [windowTitle := "hsConway",
                windowDefaultWidth := (300), windowDefaultHeight := (500 ),
                containerBorderWidth := 5 ]
--    window `on` focus $ \dirtype -> putStrLn "focused!" >> return False
    frame <- frameNew
    containerAdd window table

    canvas <- drawingAreaNew

    tableAttachDefaults table startButton 0 1 0 1
    tableAttachDefaults table pauseButton 0 1 1 2
    tableAttachDefaults table loadButton  0 1 2 3
    tableAttachDefaults table frame 1 20 0 20

    containerAdd frame canvas
    widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

    widgetShowAll window 
    drawin <- widgetGetDrawWindow canvas

    curGridRef <- newIORef glider

    onClicked startButton ( startButtonHandler drawin curGridRef pauseButton )
    onClicked loadButton ( loadButtonHandler curGridRef drawin )
    onButtonPress canvas (canvasClickHandler curGridRef drawin)
    onExpose canvas (canvasExposeHandler curGridRef drawin)
    
    onDestroy window mainQuit
    mainGUI

loadButtonHandler curGridRef drawin =
  do file <- chooseFile Nothing "Select Universe" Nothing
     unless (file == Nothing) (do contents <- readFile (fromJust file)
                                  let grid = stringToGrid $ lines contents
                                  putStrLn $ show grid     
                                  writeIORef curGridRef grid
                                  let (width, height) = dim grid
                                  putStrLn $ show $ dim grid
                                  drawWindowClear drawin
                                  renderWithDrawable drawin (drawField width height)                              
                                  renderWithDrawable drawin (drawGrid grid))


canvasExposeHandler curGridRef drawin x = 
  do  curGrid <- readIORef curGridRef
      let (width, height) = dim curGrid
      drawWindowClear drawin
      renderWithDrawable drawin (drawField width height)                              
      renderWithDrawable drawin (drawGrid curGrid)
      return True


canvasClickHandler curGridRef drawin event =
  do  curGrid <- readIORef curGridRef
      let (a, b) = coordToCell  (eventX event) (eventY event)
      when   (inbounds curGrid a b) ( do let newGrid = swapGrid a b curGrid
                                         writeIORef curGridRef newGrid
                                         renderWithDrawable drawin (drawDelta(deltaGrid curGrid newGrid))
                                    )
      return True

inbounds grid  a b = a <= x && b <= y 
  where (x, y) = dim grid

pauseButtonHandler timerID  = 
  do timeoutRemove timerID


startButtonHandler drawin curGridRef button =
  do timerID <- timeoutAdd(do   curGrid <- readIORef curGridRef
                                let newGrid = nextGen curGrid 
                                renderWithDrawable drawin (drawDelta (deltaGrid curGrid newGrid))
                                writeIORef curGridRef newGrid
                                return True) 300
     onClicked button ( pauseButtonHandler  timerID )
     return ()




buttonSwitch :: Button -> String -> String -> IO ()
buttonSwitch b x y  = do
  txt <- buttonGetLabel b
  let newtxt = case txt of
                 x -> y
                 y -> x
  buttonSetLabel b newtxt


dim :: Grid -> (Int, Int)
dim gr = (w, h)
      where ((a,b), (w,h)) = bounds gr

drawDelta ::   GridDelta -> Render ()
drawDelta  gridDelta = 
    forM_ (assocs gridDelta) (\((x,y), cell) -> case cell of 
                                        Spawn -> drawCell  x y
                                        Kill  -> killCell  x y
                                        Keep  -> return () )
    >> fill

deltaGrid :: Grid -> Grid -> GridDelta
deltaGrid oldGrid newGrid = listArray c (map f (range c))
            where c = bounds newGrid
                  f (a,b) = same (oldGrid ! (a,b)) (newGrid ! (a,b)) 
                  same Alive Alive = Keep
                  same Dead Dead = Keep
                  same Dead Alive = Spawn
                  same Alive Dead = Kill

-- mapArray :: Array a -> (Array a -> Index -> b) -> Array b
-- mapArray a f = listArray c (map (\x -> f a x) (range c))

swapGrid :: Int -> Int -> Grid -> Grid
swapGrid x y gr = gr // [((x, y), newCell)]
            where newCell = invert ( gr ! (x,y) )
                  invert Alive = Dead
                  invert Dead = Alive 


coordToCell :: Double -> Double -> (Int, Int)
coordToCell  x y = ( 1 + floor (x/cellSize), 
  1 + floor (y/cellSize ))


centerCoords :: Int -> Int -> (Double, Double)
centerCoords a b = ( fromIntegral nx,  fromIntegral ny )
        where nx = cellSize * a - div cellSize 2
              ny = cellSize * b - div cellSize 2


drawCell x y = do
    let (x',y') = centerCoords  x y 
    rectangle (( x')-5) (( y')-5) 10 10
    setSourceRGBA 1 0 0 0.8
    fill


killCell  x y = do
    let (x',y') = centerCoords  x y 
    rectangle (( x')-5) (( y')-5) 10 10
    setSourceRGBA 1 1 1 1
    fill

drawGrid :: Grid -> Render ()
drawGrid grid = 
    forM_ (assocs grid) (\((x,y), cell) -> if cell == Alive then drawCell  x y else return ()) 
    >> fill

drawField :: Int -> Int -> Render ()
drawField w h = do
    setSourceRGB 0 0 0
    setLineWidth 1
    moveTo 0 0
    forM_ [1..h] $ \x -> do  moveTo 0 (fromIntegral $ x*cellSize)
                             lineTo   (fromIntegral $ w*cellSize) (fromIntegral $ x*cellSize)
    forM_ [1..w] $ \x -> do  moveTo   (fromIntegral $ x*cellSize) 0
                             lineTo   (fromIntegral $ x*cellSize) (fromIntegral $ h*cellSize)
    stroke



neighbours :: Grid -> Int -> Int -> [Cell]
neighbours grid x y = map (\x -> grid ! x ) ns
    where ns = [ (x', y') | x' <- [x-1, x, x+1], y' <- [y-1, y, y+1], not(x'==x && y' == y), inbounds (x', y')]
          ((x1,y1),(x2,y2)) = bounds grid
          inbounds (a,b)    = x1 <= a && a <= x2 && y1 <= b && b <= y2


neighbourCount :: Grid -> Int -> Int -> Int
neighbourCount grid x y  = sum (map trans (neighbours grid x y))
                    where trans Dead = 0
                          trans Alive  = 1

action :: Cell -> Int -> Cell
action Alive nghc 
    | nghc <= 1 = Dead
    | nghc == 2 = Alive
    | nghc == 3 = Alive
    | nghc > 3  = Dead
action Dead nghc
    | nghc == 3 = Alive 
    | otherwise = Dead

nextGen :: Grid -> Grid
nextGen gr = listArray b (map f (range b))
                where b = bounds gr
                      f (x,y) = action (gr ! (x,y)) (neighbourCount gr x y)


stringToGrid :: [String] -> Grid
stringToGrid s = listArray ((1,1), (h, w)) $ linearize s -- (1,1) (1,2) (1,3) .. (2,1) ..
        where w = length (head s)
              h = length s

linearize s = concatMap f s
        where f         = map trans
              trans '.' = Dead
              trans _   = Alive

gridToString :: Grid -> [String]
gridToString gr = map l [1..height]
        where ((a,b), (height, width)) = bounds gr
              l i = gridLineString gr i
              gridLineString gr i = concatMap trans [ gr ! (i, x) | x <- [1..width]]
              trans Dead = "."
              trans Alive  = "#"


conc (x:xs) a = r:(conc xs r)
    where r = x a
conc [] a = [] 

gridList gr = conc (repeat nextGen) gr

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


chooseFile :: Maybe Window -> String -> Maybe FilePath -> IO (Maybe FilePath)
chooseFile window prompt mbFolder = do
    dialog <- fileChooserDialogNew
                    (Just $ prompt)
                    ( window)
                FileChooserActionOpen
                [("gtk-cancel"
                ,ResponseCancel)
                ,("gtk-open"
                ,ResponseAccept)]
    when (isJust mbFolder) $ fileChooserSetCurrentFolder dialog (fromJust mbFolder)  >> return ()
    widgetShow dialog
    response <- dialogRun dialog
    case response of
        ResponseAccept -> do
            fn <- fileChooserGetFilename dialog
            widgetDestroy dialog
            return fn
        ResponseCancel -> do
            widgetDestroy dialog
            return Nothing
        ResponseDeleteEvent -> do
            widgetDestroy dialog
            return Nothing
        _                   -> return Nothing