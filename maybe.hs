module Main where

data May a = J a | N deriving (Show)

class Mo m where
	bi :: m a -> (a -> m b) -> m b
	s :: m a -> m b -> m b
	r :: a -> m a

instance Mo May where
	bi (J a) f = f a
	bi N f = N
	s m n = n
	r a = J a

p :: (Show a) => May a -> String
p (J a) = "J " ++ show a
p N = "N"

main = return ()
