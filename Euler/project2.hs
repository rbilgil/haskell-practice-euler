import Data.Scientific

fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

allFibs = map fib [0..]
fibs x = take x allFibs

fibsLessThan x = takeWhile (<x) allFibs

evens = filter (\x -> even x)

main = do
	let evenFibs = evens (fibsLessThan $ floor 4.0e6)
	print $ sum evenFibs