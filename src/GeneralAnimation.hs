
import Data.Monoid


data  (Dyna a) = Dyna (Float -> a)

instance (Monoid a) => Monoid (Dyna a) where
	mempty  = mempty
	mappend (Dyna f) (Dyna g) = Dyna (\t -> (mappend f g) t)

df1 = Dyna (\t -> [1])

df2 = Dyna (\t -> [2])

s = df1 <> df2

run (Dyna f) t  = f t  

main = do putStrLn $ show $ (run s) 1