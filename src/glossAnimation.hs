-- ghc -prof -auto-all -o gloss glossModular.hs -O2 -threaded
-- ./gloss +RTS -p
-- somehow only really works with -O2


{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances#-}
import Graphics.Gloss.Interface.Pure.Animate hiding (dim)
import Data.Maybe
import Data.List (unlines, lines, intersperse)
import Control.Monad (forM_, when, unless, liftM,mplus)
import Conway
import GeneralAnimation
import System.Environment (getArgs)
import Data.Semigroup

type Animation = Dynamic Float Picture
instance Semigroup Picture where
    a <> b = pictures [a, b]


cellSize = 10

maybeFirstArg = do list <- getArgs 
                   let z = bang list 
                   return z
      where bang  [] = Nothing
            bang  (x:xs) = Just x

gliderFromFile filename = do content <- readFile filename 
                             return $ stringToGrid $ lines content

main = do
   mFilename <- maybeFirstArg
   let defaultfilename = Just "../universes/line.conway"
   g <- gliderFromFile (fromJust $ mplus mFilename defaultfilename)
   let (width, height) = dim g
       w = fromIntegral $ cellSize*width
       h = fromIntegral $ cellSize*height

   animate ( InWindow "foo" (w+10,h+10) (w,h) ) black 
      (\x -> anim ( trans (-w/2) (-h/2) $ combConst (animationList g) (border w h) ) (2*x))

trans w h ani  = transformAnimations (translate w h) ani 

border w h = translate (w/2) (h/2) $ color white $ lineLoop $ rectanglePath  w h
s1 = 2
s2 = 4
spawn :: Int -> Int -> Float -> Picture
spawn x y time = color (redish (time))  $
      translate (centerX x y) (centerY x y) (thickCircle s1 s2)
kill x y  time = color (redish (1-time)) $ 
      translate (centerX x y) (centerY x y) (thickCircle s1 s2)
keepAlive x y  time = color red $ 
      translate (centerX x y) (centerY x y) (thickCircle s1 s2)
keepDead x y time = blank
redish t = makeColor (t) 0 0 1 


centerCoords :: Int -> Int -> (Float,Float)
centerCoords a b = ( fromIntegral nx,  fromIntegral ny )
        where nx = cellSize * a - div cellSize 2
              ny = cellSize * b - div cellSize 2

centerX a b = fst $ centerCoords a b 
centerY a b = snd $ centerCoords a b 

ft :: Float
ft = 1
transf :: GridDelta -> [Animation]
transf g = traverseFilter g tr kd 
  where tr ((x,y), Kill)      = mkDynamic ft (kill  x y) 
        tr ((x,y), Spawn)     = mkDynamic ft (spawn x y)
        tr ((x,y), KeepDead)  = mkDynamic ft (keepDead x y)
        tr ((x,y), KeepAlive) = mkDynamic ft (keepAlive x y)
        kd ((x,y), KeepDead) = False
        kd _ = True


animationList gr = map (\g -> combAnim (transf g) ) (deltaGridList gr)