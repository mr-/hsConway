import GeneralAnimation
import Data.Semigroup

instance Semigroup Double where
	(<>) = (+)

n = 4 :: Double
m = 5 :: Double 
main = putStrLn $ show $ anim (combConst  [d, e, f] (-0)) 0.5

d :: Dynamic Double Double
d = mkDynamic 1 (\t -> t )
e = mkDynamic 1 (\t -> t)
f = mkDynamic 1 (\t -> t +10000)

