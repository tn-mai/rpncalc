import System.Environment (getArgs)
import Text.Read (readMaybe)
import Control.Monad (foldM, liftM)

main :: IO ()
main = do
  s <- getArgs
  let r = foldM calcurate [] . words $ head s
  case r of
    Left e  -> putStrLn $ "Parse error in '" ++ e ++ "'."
    Right n -> putStrLn . show $ head n

calcurate :: [Double] -> String -> (Either String [Double])
calcurate xs "sum" = Right [sum xs]
calcurate (x:xs) "ln" = Right $ log x:xs
calcurate (x1:x2:xs) "^" = Right $ (x2 ** x1):xs
calcurate (x1:x2:xs) "*" = Right $ (x2 * x1):xs
calcurate (x1:x2:xs) "/" = Right $ (x2 / x1):xs
calcurate (x1:x2:xs) "+" = Right $ (x2 + x1):xs
calcurate (x1:x2:xs) "-" = Right $ (x2 - x1):xs
calcurate xs e = liftM (:xs) $ read e
  where
    read :: (Read a) => String -> Either String a
    read e = case reads e of
      [(x, "")] -> Right x
      _ -> Left $ "Parse error in '" ++ e ++ "'"
