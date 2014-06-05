-- these are equivalent
add x y = x+y
add' x = \y -> x + y
add'' = \x y -> x + y
add''' = \x -> \ y -> x + y

-- equivalent to class Ration { R(int, int) }
-- (the constructor name isn't the same as the type name,
-- as it is in Java/C#) (also, data types, type classes and constructors
-- need to start with capitals)
data -- keyword
	Ration =  --data type name
				R -- data ctor name
					Int -- 1st constructor arg type
					Int -- 2nd constructor arg type
						deriving -- keyword
						  Show  -- type class, similar to interface

						  
addRation :: Ration -> Ration -> Ration
addRation (R n m) (R x y) = R (n+x) (m+y)

-- equivalent to addRation
addRation' :: Ration -> Ration -> Ration
addRation' a b = 
	case (a, b) of
	  (R n m, R x y) -> R (n + x) (m + y)
	  
	  
getOr :: Maybe a -> a -> a
getOr Nothing dfault = dfault
getOr (Just x) dfault = x

-- e.g. use withIt (Just 10) (\x -> x + 3)
withIt :: Maybe a -> (a -> b) -> Maybe b
withIt Nothing _ = Nothing
withIt (Just x) f = Just (f x)
