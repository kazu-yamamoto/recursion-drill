gt :: Int -> Int -> Bool
gt 0 _ = False
gt _ 0 = True
gt m n = gt (m - 1) (n - 1)

gteq :: Int -> Int -> Bool
gteq _ 0 = True
gteq 0 _ = False
gteq m n = gteq (m - 1) (n - 1)

isOdd :: Int -> Bool
isOdd 0 = False
isOdd 1 = True
isOdd n = isOdd (n - 2)

remainder :: Int -> Int -> Int
remainder m n
  | m < n     = m
  | otherwise = remainder (m - n) n

devide2 :: Int -> Int -> Int
devide2 m n = devide2' m n 0

devide2' :: Int -> Int -> Int -> Int
devide2' n m acc
  | n < m     = acc
  | otherwise = devide2' (n - m) m (acc + 1)
