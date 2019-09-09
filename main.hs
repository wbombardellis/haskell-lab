module Main (main) where

data Inst a = Caneta a | Lapiz a deriving Show

class Doable a where
	do_ :: a -> a

instance Doable Integer where
	do_ a = a + 1

instance Doable [Char] where
	do_ a = a ++ "!"

main = (getChar >>= (\a -> putChar a))
	>> putStr "bye, bye"
	>> print "kk"
