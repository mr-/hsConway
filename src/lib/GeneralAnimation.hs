{-# LANGUAGE DeriveFunctor
           , GeneralizedNewtypeDeriving
           , TypeSynonymInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , FlexibleInstances
  #-}
module GeneralAnimation where

import Data.Semigroup
import Data.List
--Want newtype Animation = Dynamic Double Picture
-- have Time and Duration seperate..

--type Animation = Dynamic Double (Render ())

data Dynamic t a = Dynamic {duration :: t, runDynamic :: (t -> a) }

instance (Semigroup a, Ord t) => Semigroup (Dynamic t a) where
    x <> y = Dynamic t (\t -> (fx t) <> (fy t))
        where t  = max (duration x) (duration y)
              fx = runDynamic x
              fy = runDynamic y 

mkDynamic :: t -> (t -> a) -> Dynamic t a
mkDynamic = Dynamic 


combine :: (Semigroup a, Num t) => [(t -> a)] -> t -> a
combine = foldl1 (<>) 


anim :: (Semigroup a, Ord t, Num t) => [Dynamic t a] -> t -> a
anim l t = (runDynamic func) (t - (duration func) + stime)
    where func = find' t startTimed 
          stime = duration (head l)
          startTimed = scanl1 addTimes l
          addTimes a b = Dynamic ((duration a) + (duration b)) (runDynamic b)
          find' t li = head $ dropWhile (\x -> ( duration x )<= t) li

--combAnim :: [Animation] -> Animation
combAnim l = Dynamic m (combine l')
  where m  = maximum $ map duration l
        l' = map runDynamic l 

combConst :: (Semigroup a) => [Dynamic t a] -> a -> [Dynamic t a]
combConst l pic = map (combConst1 pic) l

combConst1 :: (Semigroup a) => a -> (Dynamic t a) -> (Dynamic t a)
combConst1 pic d = Dynamic (duration d) f
  where f = \t -> ((runDynamic d) t) <> pic

-- would rather have pic to be a constant dynamic and just <> those..        


funcFromDynamics  = runDynamic 
timeFromDynamics  = duration 


transformAnimation :: (a -> a) -> Dynamic t a -> Dynamic t a
transformAnimation tr an = Dynamic (duration an) (\t -> tr ( (runDynamic an) t))

transformAnimations tr an = map (transformAnimation tr) an



