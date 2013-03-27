
{-# LANGUAGE FlexibleInstances #-}


import Data.Semigroup

--Want newtype Animation = Dynamic Double Picture

data Time a = Forever | Duration a 

--add :: (Num a) => (Time a) -> (Time a) -> (Time a)
add Forever _ = Forever
add _ Forever = Forever
add (Duration x) (Duration y) = x + y

sub Forever _ = Forever
sub _ Forever = 0
sub (Duration x) (Duration y) = x - y

instance (Eq a) => Eq (Time a) where
    Forever == Forever       =  True
    (Duration a) == (Duration b)  =  a == b
    _ == _             =  False

instance (Ord a) => Ord (Time a) where
    compare _ Forever                 = LT
    compare Forever _                 = GT
    compare (Duration a) (Duration b) = compare a b


data Dynamic t a = Dynamic {duration :: Time t, runDynamic :: (Time t -> a) }

instance (Semigroup a, Ord t) => Semigroup (Dynamic t a) where
    x <> y = Dynamic t (\t -> (fx t) <> (fy t))
        where t  = max (duration x) (duration y)
              fx = runDynamic x
              fy = runDynamic y 

combine :: (Semigroup a, Ord t) => [(Time t -> a)] -> Time t -> a
combine   = foldl1 (<>) 

anim :: (Semigroup a, Ord t, Num t) => [Dynamic t a] -> Time t -> a
anim l t  =  f (t `sub` s)
  where ((s, e), f) = fromDur l t
        fromDur l t = head $ filter (\((s,e), f) -> and [(s <= t),(t <= e)] ) (totalFromDur l)
        totalFromDur l = scanl calc ((Duration 0, duration dy), runDynamic dy) l 
            where dy = head l
                  calc ( (startac, endac), fac) d = ((endac, (duration d) `add` endac), runDynamic d)

{-
combConst :: [Dynamic t a] -> a -> [Dynamic t a]
combConst l pic = map tr l
  where tr (A (t, f)) = A (t, combine [f, (\t -> pic)])

funcFromAnimation (A (x, f)) = f
timeFromAnimation (A (x, f)) = x

transformAnimation :: (Picture -> Picture) -> Animation -> Animation
transformAnimation tr an = A(x, \t -> tr (f t) )
   where f = funcFromAnimation an 
         x = timeFromAnimation an 

transformAnimations :: (Picture -> Picture) -> [Animation] -> [Animation]
transformAnimations tr an = map (transformAnimation tr) an






combAnim :: [Animation] -> Animation
combAnim l = A (m, combine l')
  where m  = maximum $ map timeFromAnimation l
        l' = map funcFromAnimation l 


-}