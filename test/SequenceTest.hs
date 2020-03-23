module Main where

import Control.Applicative

exampleList = [1,2] ++ exampleList


-- Example handle'ing PatternMatchFail using the ST monad and maybe IoToSt

main :: IO ()
main = test2

test2 = do
    
exampleST = 

test1 = do
	print $ take 4 exampleList
	let newList1 = tail exampleList
	print $ take 4 newList1
	let newList2 = tail newList1
	print $ take 4 newList2
	let jarl = [1, 2]
	let funs = [(* 3), (* 4)]
	print $ map (\(f,x) -> f x) (zip funs jarl)