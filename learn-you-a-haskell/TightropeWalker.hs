module Main
  ( main
  ) where

type Birds = Int

-- represent how many birds are on the left and on the right side of the pole.
type Pole = (Birds, Birds)

(-:) :: a -> (a -> b) -> b
x -: f = f x

landLeft :: Birds -> Pole -> Pole
landLeft n (left, right) = (left + n, right)

landRight :: Birds -> Pole -> Pole
landRight n (left, right) = (left, right + n)

landLeftMaybe :: Birds -> Pole -> Maybe Pole
landLeftMaybe n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRightMaybe :: Birds -> Pole -> Maybe Pole
landRightMaybe n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

landLeftEither :: Birds -> Pole -> Either String Pole
landLeftEither n (left, right)
  | abs (left' - right) < 4 = Right (left', right)
  | otherwise =
    Left $
    "Pierre drop with " ++
    show left' ++
    " birds on the left and " ++ show right ++ " birds on the right."
  where
    left' = left + n

landRightEither :: Birds -> Pole -> Either String Pole
landRightEither n (left, right)
  | abs (left - right') < 4 = Right (left, right')
  | otherwise =
    Left $
    "Pierre drop with " ++
    show left ++
    " birds on the left and " ++ show right' ++ " birds on the right."
  where
    right' = right + n

main :: IO ()
main = do
  print $ landLeft 2 (landRight 1 (landLeft 1 (0, 0)))
  -- with (-:) this becomes
  print $ (0, 0) -: landLeft 1 -: landRight 1 -: landLeft 2
  -- here Pierre should fall in the way
  print $ (0, 0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)
  -- Maybe allows failure on the way
  print $ return (0, 0) >>= landLeftMaybe 1 >>= landRightMaybe 1 >>= landLeftMaybe 2
  print $ return (0, 0) >>= landLeftMaybe 1 >>= landRightMaybe 4 >>= landLeftMaybe (-1) >>= landRightMaybe (-2)
  print $ return (0, 0) >>= landLeftMaybe 1 >> Nothing >>= landRightMaybe 4
  -- Illustration of the do notation. Not needed here since it steps depends on the previous result.
  print $ do
    start <- return (0, 0)
    first <- landLeftMaybe 1 start
    second <- landRightMaybe 1 first
    landLeftMaybe 2 second
  print $ do
    start <- return (0, 0)
    first <- landLeftMaybe 1 start
    Nothing
    second <- landRightMaybe 1 first
    landLeftMaybe 2 second
  -- Either enables error information
  print $ return (0, 0) >>= landLeftEither 1 >>= landRightEither 1 >>= landLeftEither 2
  print $ return (0, 0) >>= landLeftEither 1 >>= landRightEither 4 >>= landLeftEither (-1) >>= landRightEither (-2)
  print $ return (0, 0) >>= landLeftEither 1 >> Left "BANANA!!!" >>= landRightEither 4
