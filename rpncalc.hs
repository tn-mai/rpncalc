import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  s <- getArgs
  let r = foldl calcurate (Right []) . words $ head s
  case r of
    Left e  -> putStrLn $ "Parse error in '" ++ e ++ "'."
    Right n -> putStrLn . show $ head n

calcurate :: (Either String [Double]) -> String -> (Either String [Double])
calcurate (Left e) _ = Left e
calcurate (Right xs) "sum" = Right [sum xs]
calcurate (Right (x:xs)) "ln" = Right $ log x:xs
calcurate (Right (x1:x2:xs)) "^" = Right $ (x2 ** x1):xs
calcurate (Right (x1:x2:xs)) "*" = Right $ (x2 * x1):xs
calcurate (Right (x1:x2:xs)) "/" = Right $ (x2 / x1):xs
calcurate (Right (x1:x2:xs)) "+" = Right $ (x2 + x1):xs
calcurate (Right (x1:x2:xs)) "-" = Right $ (x2 - x1):xs
calcurate (Right xs) e = case readMaybe e of
  Just n -> Right $ n:xs
  Nothing -> Left e

