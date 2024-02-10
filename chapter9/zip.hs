zip2 :: [a] ->[b] -> [(a,b)]
zip2 [] _ = []
zip2 _ [] = []
zip2 (x:xs) (y:ys) = [(x, y)] ++ (zip2 xs ys)

zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2 f [] _ = []
zipWith2 f _ [] = []
zipWith2 f (x:xs) (y:ys) = (f x y) : (zipWith2 f xs ys)

-- TODO, make the function down-below work
{-
zip3 :: [a] -> [b] -> [(a,b)]
zip3 x y = ((zipWith2 fst x y), (zipWith2 snd x y))
-}