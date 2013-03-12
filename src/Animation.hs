
module Animation where
import Graphics.Gloss.Interface.Pure.Animate
import Data.Array
import Data.List (unlines, lines, intersperse)

data Animation = A (Float, Float -> Picture)

instance Show Animation where
  show (A (i, f)) = "A " ++ (show i) ++ "f" 

combConst :: [Animation] -> Picture -> [Animation]
combConst l pic = map tr l
  where tr (A (t, f)) = A (t, combine [f, (\t -> pic)])

funcFromAnimation (A (x, f)) = f
timeFromAnimation (A (x, f)) = x



anim :: [Animation] -> Float -> Picture
anim l t  = a (t - start)
  where ((start, end), a) = fromDur t l

fromDur :: Float -> [Animation] -> ((Float, Float), Float -> Picture)
fromDur t l = head $ filter (\((s,e), f) -> and [(s <= t),(t <= e)] ) (totalFromDur l)

totalFromDur :: [Animation] -> [((Float, Float), Float -> Picture)]
totalFromDur l = scanl calc ((0,s),abc) (tail l)
    where A (s, abc) = head l
          calc ( (startac, endac), fac) (A (ordur, for)) = ((endac, ordur + endac), for)


combAnim :: [Animation] -> Animation
combAnim l = A (m, combine l')
  where m  = maximum $ map timeFromAnimation l
        l' = map funcFromAnimation l 

combine :: [(Float -> Picture)] -> Float -> Picture
combine l t = pictures $ map (\a -> a t) l 
