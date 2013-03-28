-- ghc -prof -auto-all -o gloss glossModular.hs -O2 -threaded
-- ./gloss +RTS -p
-- somehow only really works with -O2


{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances#-}
import Graphics.Gloss.Interface.Pure.Animate hiding (dim)
import Data.Maybe
import Data.List (unlines, lines, intersperse)
import Control.Monad (forM_, when, unless, liftM,mplus)
import Conway
import Data.Active
import System.Environment (getArgs)
import Data.Semigroup

--type Animation = Dynamic Float Picture
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
   let defaultfilename = Just "../../universes/line.conway"
   g <- gliderFromFile (fromJust $ mplus mFilename defaultfilename)
   let (width, height) = dim g
       w = fromIntegral $ cellSize*width
       h = fromIntegral $ cellSize*height

   animate ( InWindow "foo" (w+10,h+10) (w,h) ) black 
--      (\x -> anim ( trans (-w/2) (-h/2) $ combConst (animationList g) (border w h) ) (2*x))
      (\x -> runActive ( movie (take 50 (animationList g))) (toTime (2*x)))



--trans w h ani  = transformAnimations (translate w h) ani 


border w h = translate (w/2) (h/2) $ color white $ lineLoop $ rectanglePath  w h

spawn x y time      = color (redish (time))   $ tr' x y  cell
kill x y  time      = color (redish (1-time)) $ tr' x y  cell
keepAlive x y  time = color red               $ tr' x y  cell
keepDead x y time   = blank

redish t = makeColor (t) 0 0 1 
cell = thickCircle 2 4
tr' x y = translate (centerX x y) (centerY x y)
  where
    centerX a b = fst $ centerCoords a b 
    centerY a b = snd $ centerCoords a b 
    centerCoords a b = ( fromIntegral nx,  fromIntegral ny )
        where nx = cellSize * a - div cellSize 2
              ny = cellSize * b - div cellSize 2


ft :: Float
ft = 1
transf :: GridDelta -> [Active Picture]
transf g = traverseFilter g tr kd 
  where tr ((x,y), Kill)      = uh (kill  x y) 
        tr ((x,y), Spawn)     = uh (spawn x y)
        tr ((x,y), KeepDead)  = uh (keepDead x y)
        tr ((x,y), KeepAlive) = uh (keepAlive x y)
        kd ((x,y), KeepDead) = False
        kd _ = True
        uh x = mkActive z o (\t -> x  (fromTime t))
        z = toTime 0
        o = toTime 1


animationList :: Grid -> [Active Picture]
animationList gr = 
  map (\g -> foldl1 (<>) (transf g) ) (deltaGridList gr)