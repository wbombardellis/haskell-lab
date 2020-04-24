a :: Integer -> Integer -> Integer
a 0 y = y + 1
a x 0 = a (x-1) 1
a x y = a (x-1) (a x (y-1))
