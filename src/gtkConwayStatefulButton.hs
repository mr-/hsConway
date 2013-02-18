{-# LANGUAGE NoMonomorphismRestriction #-}

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events
import Data.Array.Unboxed
import Data.List
import Control.Monad.State
import Data.IORef
import Control.Concurrent as CC

type Grid = UArray (Int,Int) Bool
type GridDelta = UArray (Int,Int) Int

wWidth = 500 
wHeight = 500
height = 30
width = 30

main :: IO ()
main= do
    initGUI
    window <- windowNew
    hbox    <- hBoxNew False 10
    button1 <- buttonNewWithLabel "Start"

    set window [windowTitle := "Hello Cairo",
                windowDefaultWidth := (wWidth + 10+50), windowDefaultHeight := (wHeight + 10),
                containerBorderWidth := 5, containerChild := hbox ]
--    window `on` focus $ \dirtype -> putStrLn "focused!" >> return False
    frame <- frameNew
    containerAdd window frame
    canvas <- drawingAreaNew
    boxPackStart hbox button1 PackNatural 0
    boxPackStart hbox frame PackGrow 0

    containerAdd frame canvas
    widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

    widgetShowAll window 
    drawin <- widgetGetDrawWindow canvas

    curGridRef <- newIORef glider

    timeoutAdd ( do curGrid <- readIORef curGridRef
                    let newGrid = nextGen curGrid 
                    renderWithDrawable drawin (mainloop (delta curGrid newGrid))
                    writeIORef curGridRef newGrid
                    return True) 1000

    onButtonPress canvas 
                (\x -> do   let (a, b) = coordToCell (eventX x) (eventY x)
                            curGrid <- readIORef curGridRef
                            let newGrid = swap a b curGrid
                            writeIORef curGridRef newGrid
                            renderWithDrawable drawin (mainloop (delta curGrid newGrid))
                            return True)

    onExpose canvas (\x -> do renderWithDrawable drawin drawField
                              curGrid <- readIORef curGridRef
                              renderWithDrawable drawin (drawGrid curGrid)
                              return True )
    
    onDestroy window mainQuit
    mainGUI


mainloop :: GridDelta -> Render ()
mainloop gridDelta = do
    let b = bounds gridDelta
    forM_ (range b) (\(x,y) -> if gridDelta ! (x,y) == 2 then 
                                    drawCell x y 
                                else ( if gridDelta ! (x,y) == 1 then 
                                    killCell x y 
                                else return () ) )
    fill

delta :: Grid -> Grid -> GridDelta
delta oldGrid newGrid = listArray c (map f (range c))
            where c = bounds newGrid
                  f (a,b) = if newGrid ! (a,b) == oldGrid ! (a,b) then 0 else ( if (oldGrid ! (a,b)) then 1 else 2) 

-- 0 don't do anything
-- 1 was alive - kill now
-- 2 was dead - spawn

swap :: Int -> Int -> Grid -> Grid
swap x y gr = listArray c (map f (range c))
            where c = bounds gr
                  f (a,b) = if (x,y) == (a,b) then (not (gr ! (x,y))) else ((gr ! (a,b))) 


coordToCell :: Double -> Double -> (Int, Int)
coordToCell x y = ( 1 + floor (x/(wWidth/width)), 1 + floor (y/(wHeight/height)) )


centerCoords :: Int -> Int -> (Double, Double)
centerCoords a b = ( fromInteger $ floor $ nx,  fromInteger $ floor $ ny )
        where  ydel =  wHeight/height
               xdel =  wWidth/width
               nx =    ((toRational a)-1)*xdel + xdel/2
               ny =    ((toRational b) -1)*ydel + ydel/2 

reset = do
    rectangle 0 0 wWidth wHeight
    setSourceRGBA 1 1 1 1
    fill

drawOnClick a b = do
    let (a', b') = coordToCell a b
    let (x,y) = centerCoords a' b'       
    rectangle (( x)-15) (( y)-15) 30 30
    setSourceRGBA 1 0 0 0.8
    fill

drawCell x y = do
    let (x',y') = centerCoords x y 
    rectangle (( x')-5) (( y')-5) 10 10
    setSourceRGBA 1 0 0 0.8
    fill

drawGrid :: Grid -> Render ()
drawGrid grid = do
    let b = bounds grid
  --  drawCell 1 1
    mapM_ (\(x,y) -> if grid ! (x,y) == True then drawCell x y else return ()) (range b)
    fill

killCell x y = do
    let (x',y') = centerCoords x y 
    rectangle (( x')-5) (( y')-5) 10 10
    setSourceRGBA 1 1 1 1
    fill

drawField :: Render ()
drawField = do
    setSourceRGB 0 0 0
    setLineWidth 1
    forM_ [1..height] $ \x -> do  moveTo 0 (x*wHeight/height)
                                  lineTo wHeight (x*wHeight/height)
    forM_ [1..width] $ \x -> do  moveTo (x*wWidth/width) 0 
                                 lineTo (x*wWidth/width) wWidth
    stroke



neighbours :: Grid -> Int -> Int -> [Bool]
neighbours grid x y = map (\x -> grid ! x ) ns
    where ns = [ (x', y') | x' <- [x-1, x, x+1], y' <- [y-1, y, y+1], not(x'==x && y' == y), inbounds (x', y')]
          ((x1,y1),(x2,y2)) = bounds grid
          inbounds (a,b)    = x1 <= a && a <= x2 && y1 <= b && b <= y2


neighbourCount :: Grid -> Int -> Int -> Int
neighbourCount grid x y  = sum (map trans (neighbours grid x y))
                    where trans False = 0
                          trans True  = 1

action :: Bool -> Int -> Bool
action True nghc 
    | nghc <= 1 = False
    | nghc == 2 = True
    | nghc == 3 = True
    | nghc > 3  = False
action False nghc
    | nghc == 3 = True 
    | otherwise = False

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
              trans '.' = False
              trans _   = True

gridToString :: Grid -> [String]
gridToString gr = map l [1..height]
        where ((a,b), (height, width)) = bounds gr
              l i = gridLineString gr i

              gridLineString gr i = concatMap trans [ gr ! (i, x) | x <- [1..width]]
              trans False = "."
              trans True  = "#"


conc (x:xs) a = r:(conc xs r)
    where r = x a
conc [] a = [] 

gridList gr = conc (repeat nextGen) gr

glider = stringToGrid
   ["..............................",
    "..............................",
    "..............................",
    "..............................",
    "..............................",
    "..............................",
    "..............................",
    "..............................",
    "..............................",
    "..............................",
    "..............................",
    "..............................",
    "..............................",
    "..............................",
    "..............................",
    "..............................",
    ".....#........................",
    ".....#...###..................",
    ".....#........................",
    "..............................",
    "..............................",
    "..............................",
    "..............................",
    "..###.........................",
    "..#.#.........................",
    "...#..........................",
    "...#.......................###",
    "...........................#..",
    "............................#.",
    ".............................."]
