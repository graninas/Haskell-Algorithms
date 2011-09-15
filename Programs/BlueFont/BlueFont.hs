

process [] _ = []
process ('|':xs) True  = "<font color=blue>" ++ process xs False
process ('|':xs) False = "</font>" ++ process xs True
process (x:xs) b = x : process xs b

main :: IO ()
main = do
	fContents <- readFile "text.txt"
	let processed = process fContents True
	writeFile "p_text.txt" processed
	putStrLn "Ok."