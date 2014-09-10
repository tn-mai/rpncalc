import System.Environment (getArgs)

main :: IO ()
main = do
  s <- getArgs
  let r = calc $ head s
  case r of
    Just value -> putStrLn $ show value
    Nothing -> putStrLn "PARSE ERROR."

calc :: String -> Maybe (Double)
calc s = parse [] $ tokenize s

parse :: [Double] -> [String] -> Maybe (Double)
parse [] [] = Nothing
parse stack [] = Just $ head stack
parse (stack1:stack2:stackTail) ("*":xs) = parse ((stack2 * stack1) : stackTail) xs
parse (stack1:stack2:stackTail) ("/":xs) = parse ((stack2 / stack1) : stackTail) xs
parse (stack1:stack2:stackTail) ("+":xs) = parse ((stack2 + stack1) : stackTail) xs
parse (stack1:stack2:stackTail) ("-":xs) = parse ((stack2 - stack1) : stackTail) xs
parse stack (x:xs) = parse (read x:stack) xs

tokenize :: String -> [String]
tokenize s = foldr (\e (x:xs) -> if e == ' ' then []:x:xs else (e:x):xs) [[]] s
