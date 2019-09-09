
par :: Integral a => a -> IO ()
par n = if n `mod` 2 == 0 then putStrLn "par" else putStrLn "impar"

run :: (Show a, Integral a) => a -> IO ()
run a = let b = a `div` 10 in
		if b >= 1 then 
			do
				putStrLn (show b)
				par b
				run b
		else
			return ()

main = do
	putStrLn "Ola, qual e a sua idade? "
	age <- getLine
	
	let a = read age in
		run a

