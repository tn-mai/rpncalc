import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  s <- getArgs
  let r = foldl calcurate (Just []) . words $ head s
  case r of
    Nothing -> putStrLn "PARSE ERROR."
    Just n  -> putStrLn . show $ head n

calcurate :: Maybe ([Double]) -> String -> Maybe ([Double])
calcurate Nothing _ = Nothing
calcurate (Just xs) "sum" = Just [sum xs]
calcurate (Just (x:xs)) "ln" = Just $ log x:xs
calcurate (Just (x1:x2:xs)) "^" = Just $ (x2 ** x1):xs
calcurate (Just (x1:x2:xs)) "*" = Just $ (x2 * x1):xs
calcurate (Just (x1:x2:xs)) "/" = Just $ (x2 / x1):xs
calcurate (Just (x1:x2:xs)) "+" = Just $ (x2 + x1):xs
calcurate (Just (x1:x2:xs)) "-" = Just $ (x2 - x1):xs
calcurate (Just xs) e = case readMaybe e of
  Just n -> Just $ n:xs
  Nothing -> Nothing

