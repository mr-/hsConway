-- ghc -prof -auto-all -o gloss glossModular.hs -O2 -threaded
-- ./gloss +RTS -p
-- somehow only really works with -O2


{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances#-}

import Data.List (unlines, lines, intersperse)
import Control.Monad (forM_, when, unless, liftM, forever)
import Control.Monad.State
import Conway
import Control.Concurrent (threadDelay)
main :: IO ()
main = do
   runStateT (forever $ (foo >> (liftIO $ threadDelay 200000) )) 0 >> return ()

foo :: StateT Int IO ()
foo = do
   n <- get
   liftIO $ putStrLn $ unlines $ deltaToString $ (deltaGridList grid4) !! n
   put (n+1)


grid5 = stringToGrid [
                     ".....", 
                     "..#..",
                     "..#..",
                     "..#..",
                     "....."]

grid3 = stringToGrid ["..........#..........", 
                     "..........#..........", 
                     "..........#..........", 
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#.........."]
grid4 = stringToGrid [
                     "...............................#..............................", 
                     "...............................#..............................", 
                     "...............................#..............................", 
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................", 
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "##############################################################",
                     "...............................#..............................",
                     "...............................#..............................", 
                     "...............................#..............................", 
                     "...............................#..............................", 
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................", 
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "##############################################################",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#..............................",
                     "...............................#.............................."
                     ]
grid = stringToGrid [
                     "..............................#..............................", 
                     "..............................#..............................", 
                     "..............................#..............................", 
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................", 
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     ".............................................................",
                     ".............................................................",
                     ".............................................................",
                     ".............................................................",
                     ".............................................................",
                     ".............................................................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "############......########################......#############",
                     "..............................#..............................", 
                     "..............................#..............................", 
                     "..............................#..............................", 
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     ".............................................................",
                     ".............................................................", 
                     ".............................................................",
                     ".............................................................",
                     ".............................................................",
                     ".............................................................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#..............................",
                     "..............................#.............................."
                     ]

grid2 = stringToGrid [
                     "..........", 
                     "..........", 
                     "..........", 
                     "..........",
                     "..........",
                     ".......#..",
                     "......#...",
                     "......###.",
                     "..........",
                     ".........."]
