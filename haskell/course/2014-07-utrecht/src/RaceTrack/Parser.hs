module Parser where


readFloat :: String -> Float
readFloat str = case reads str of
	[] -> error "not a floating point number"
	(p,_):_ -> p

readInt :: String -> Int
readInt str = case reads str of
	[] -> error "not an integer"
	(p,_):_ -> p
