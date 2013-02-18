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

    xRef <- newIORef 1
    timeoutAdd ( do x <- readIORef xRef 
                    renderWithDrawable drawin (mainloop (gridList glider) x)
                    modifyIORef xRef (+1)
                    return True) 100

    onButtonPress canvas 
                (\x -> renderWithDrawable drawin (drawOnClick (eventX x) (eventY x)) >> return True)

--     onExpose canvas (\x -> do renderWithDrawable drawin drawField
--                               renderWithDrawable drawin (drawGrid glider)
--                               return True )
    
    onDestroy window mainQuit
    mainGUI


mainloop gr i = do
    reset
    drawField
    drawGrid (gr !! i)


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
    rectangle (( x')-10) (( y')-10) 20 20
    setSourceRGBA 1 0 0 0.8
    fill

drawGrid :: Grid -> Render ()
drawGrid grid = do
    let b = bounds grid
  --  drawCell 1 1
    mapM_ (\(x,y) -> if grid ! (x,y) == True then drawCell x y else return ()) (range b)
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
