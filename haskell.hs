mults a b = filter (\x -> x `mod` a == 0 || x `mod` b == 0)

multsBelowThousand = mults 3 5 [0..999]

main = do
	print $ foldl (+) 0 multsBelowThousand