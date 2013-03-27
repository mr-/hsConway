
import Data.Monoid

df1 = \t -> [1]

df2 = \t -> [2]

s = df1 <> df2
  

main = do putStrLn $ show $ s 1