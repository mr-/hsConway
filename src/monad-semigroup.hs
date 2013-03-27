{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances #-}
import Control.Monad
import Data.Maybe
import Data.Semigroup





instance Monad m => Semigroup (m a) where
	(<>) = (>>)

--main = putStrLn $ show $ [3] <> [4]
main = (putStrLn "4") <> (putStrLn "3")