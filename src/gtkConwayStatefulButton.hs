{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.Maybe (fromJust, isJust)

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Selectors.FileChooser
import Graphics.UI.Gtk

import Data.Array
import Data.List (unlines, lines, intersperse)
import Control.Monad (forM_, when, unless, liftM)
import Data.IORef
import Control.Concurrent as CC



data Cell = Dead | Alive deriving (Eq, Show)
type Grid = Array (Int,Int) Cell

data Delta = Kill | Spawn | Keep deriving (Eq, Show)
type GridDelta = Array (Int,Int) Delta

type Universe = [Grid]

cellSize = 15
defaultSpeed = 300
main :: IO ()
main= do
    initGUI
    window <- windowNew
    table   <- tableNew 20 10 True
    startButton <- buttonNewWithLabel "Start"
    pauseButton <- buttonNewWithLabel "Pause"
    loadButton  <- buttonNewWithLabel "Load"
    saveButton  <- buttonNewWithLabel "Save"

    nextButton  <- buttonNewWithLabel "<   Next   <"
    prevButton  <- buttonNewWithLabel "> Previous >"

    adj1        <- adjustmentNew defaultSpeed 10 800 100 100 1.0
    vsc         <- hScaleNew adj1
    scaleSetDigits vsc 0


    set window [windowTitle := "hsConway",
                windowDefaultWidth := (300), windowDefaultHeight := (500 ),
                containerBorderWidth := 10 ]

    containerAdd window table

    canvas <- drawingAreaNew

    tableAttachDefaults table startButton 0 1 1 2
    tableAttachDefaults table pauseButton 1 2 1 2

    tableAttachDefaults table nextButton  0 1 3 4
    tableAttachDefaults table prevButton  1 2 3 4

    tableAttachDefaults table loadButton  0 1 5 6
    tableAttachDefaults table saveButton  1 2 5 6
    tableAttachDefaults table vsc         0 2 7 8


    tableAttachDefaults table canvas 3 10 0 20

    widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

    widgetShowAll window 
    drawin <- widgetGetDrawWindow canvas

    curGridRef <- newIORef [glider]
    speedRef <- newIORef 300

    onValueChanged adj1 $ do x <- adjustmentGetValue adj1 
                             writeIORef speedRef x

    foo curGridRef canvas

    onClicked startButton ( startButtonHandler  curGridRef drawin pauseButton speedRef )
    onClicked nextButton  ( nextButtonHandler   curGridRef drawin )
    onClicked prevButton  ( prevButtonHandler   curGridRef drawin )

    onClicked saveButton  ( saveButtonHandler   curGridRef drawin )
    onClicked loadButton  ( loadButtonHandler   curGridRef drawin )

    --onButtonPress canvas  ( canvasClickHandler  curGridRef drawin )
    onExpose canvas       ( canvasExposeHandler curGridRef drawin )
    
    onDestroy window mainQuit
    mainGUI

foo curGridRef canvas = do
  drawin <- widgetGetDrawWindow canvas
  isPressedRef <- newIORef False
  history <- newIORef []
  onButtonPress canvas 
     (\e -> do { writeIORef isPressedRef True;
                 let (x, y) = coordToCell (eventX e) (eventY e)
                 ;
                 cellClickHandler curGridRef drawin x y; 
                 modifyIORef history (pushUnique (x, y));
                 return True})
  onButtonRelease canvas 
     (\_ -> do {writeIORef isPressedRef False;
                writeIORef history [];
                return True})
  onMotionNotify canvas True 
     (\e -> do {  pressed <- readIORef isPressedRef;
                  when (pressed == True) 
                  (let (x, y) = coordToCell (eventX e) (eventY e) in
                      do oldhist <- readIORef history                       
                         modifyIORef history (pushUnique (x,y))
                         when ((head oldhist) /= (x,y)) 
                            (cellClickHandler curGridRef drawin x y >> return ())
                   );
                  return True} )


pushUnique :: Eq a => a -> [a] -> [a]
pushUnique a []     = [a]
pushUnique a f@(x:xs) = case (x == a) of
                        True  -> f
                        False -> a:f

saveButtonHandler curGridRef drawin =
  do file <- chooseSaveFile Nothing "Save Universe" Nothing
     unless (file == Nothing) (do grid <- readState curGridRef
                                  let gridString = unlines (gridToString grid)
                                  writeFile (fromJust file) (gridString)
                              )


loadButtonHandler curGridRef drawin =
  do file <- chooseFile Nothing "Load Universe" Nothing
     unless (file == Nothing) (do contents <- readFile (fromJust file)
                                  let grid = stringToGrid $ lines contents
                                  writeAllState curGridRef [grid]
                                  let (width, height) = dim grid
                                  drawWindowClear drawin
                                  renderWithDrawable drawin (drawField width height)                              
                                  renderWithDrawable drawin (drawGrid grid))


canvasExposeHandler curGridRef drawin x = 
  do  curGrid <- readState curGridRef
      let (width, height) = dim curGrid
      drawWindowClear drawin
      renderWithDrawable drawin (drawField width height)                              
      renderWithDrawable drawin (drawGrid curGrid)
      return True

cellClickHandler curGridRef drawin x y =
  do  curGrid <- readState curGridRef
      when  (inbounds curGrid x y) 
            ( do let newGrid = swapGrid x y curGrid
                 writeState curGridRef newGrid
                 renderWithDrawable drawin (drawDelta(deltaGrid curGrid newGrid))
            )
      return True

canvasClickHandler curGridRef drawin event =
  do  curGrid <- readState curGridRef
      let (a, b) = coordToCell  (eventX event) (eventY event)
      when  (inbounds curGrid a b) 
            ( do let newGrid = swapGrid a b curGrid
                 writeState curGridRef newGrid
                 renderWithDrawable drawin (drawDelta(deltaGrid curGrid newGrid))
            )
      return True

inbounds grid  a b =  1 <= a && 1 <= b && a <= x && b <= y 
  where (x, y) = dim grid

pauseButtonHandler timerID = timeoutRemove timerID


startButtonHandler curGridRef drawin button speedRef =
  do speed <- readIORef speedRef 
     timerID <- timeoutAdd(   doDrawNextStep curGridRef drawin 
                            >>    return True) (floor speed)
     onClicked button ( pauseButtonHandler  timerID )
     return ()

nextButtonHandler curGridRef drawin = doDrawNextStep curGridRef drawin

prevButtonHandler curGridRef drawin = 
  do allState <- readAllState curGridRef
     case allState of
      (cur:old:xs) -> do renderWithDrawable drawin (drawDelta (deltaGrid cur old))
                         writeAllState curGridRef (old:xs)
      otherwise      -> return () 

     
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

{-
buttonSwitch :: Button -> String -> String -> IO ()
buttonSwitch b x y  = do
  txt <- buttonGetLabel b
  let newtxt = case txt of
                 x -> y
                 y -> x
  buttonSetLabel b newtxt
-}

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
                  same Dead Dead   = Keep
                  same Dead Alive  = Spawn
                  same Alive Dead  = Kill

-- mapArray :: Array a -> (Array a -> Index -> b) -> Array b
-- mapArray a f = listArray c (map (\x -> f a x) (range c))

swapGrid :: Int -> Int -> Grid -> Grid
swapGrid x y gr = gr // [((x, y), newCell)]
            where newCell = invert ( gr ! (x,y) )
                  invert Alive = Dead
                  invert Dead  = Alive 


coordToCell :: Double -> Double -> (Int, Int)
coordToCell  x y = ( 1 + floor (x/cellSize), 1 + floor (y/cellSize) )


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
neighboursB grid x y = map (\x -> grid ! x ) ns
    where ns = [ (x', y') | x' <- [x-1, x, x+1], y' <- [y-1, y, y+1], not(x'==x && y' == y), inbounds (x', y')]
          ((x1,y1),(x2,y2)) = bounds grid
          inbounds (a,b)    = x1 <= a && a <= x2 && y1 <= b && b <= y2

neighbours grid x y = map (\x -> grid ! (f x) ) ns
    where ns = [ (x', y') | x' <- [x-1, x, x+1], y' <- [y-1, y, y+1], not(x'==x && y' == y)]
          ((x1,y1),(x2,y2)) = bounds grid
          f (a, b) = ((c a x2), (c b y2))
          c a bou | (a < 1)   = bou
                  | (a > bou) = 1
                  | otherwise = a

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


chooseSaveFile :: Maybe Window -> String -> Maybe FilePath -> IO (Maybe FilePath)
chooseSaveFile window prompt mbFolder = do
    dialog <- fileChooserDialogNew
              (Just $ prompt)
              (window)
        FileChooserActionSave
        [("gtk-cancel"
         ,ResponseCancel)
        ,("gtk-save"
          , ResponseAccept)]
    when (isJust mbFolder) $ fileChooserSetCurrentFolder dialog (fromJust mbFolder)  >> return ()
    widgetShow dialog
    res <- dialogRun dialog
    case res of
        ResponseAccept  ->  do
            mbFileName <- fileChooserGetFilename dialog
            widgetDestroy dialog
            return mbFileName
        _               ->  do
            widgetDestroy dialog
            return Nothing

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
