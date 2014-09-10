import System.Environment (getArgs)

main :: IO ()
main = do
  s <- getArgs
  putStrLn . show . head . foldl calcurate [] . words $ head s

calcurate :: [Double] -> String -> [Double]
calcurate xs "sum" = [sum xs]
calcurate (x:xs) "ln" = log x:xs
calcurate (x1:x2:xs) "^" = (x2 ** x1):xs
calcurate (x1:x2:xs) "*" = (x2 * x1):xs
calcurate (x1:x2:xs) "/" = (x2 / x1):xs
calcurate (x1:x2:xs) "+" = (x2 + x1):xs
calcurate (x1:x2:xs) "-" = (x2 - x1):xs
calcurate xs e = (read e):xs

