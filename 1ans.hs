plus :: Int -> Int -> Int
plus m 0 = m
plus m n = plus m (n - 1) + 1

minus :: Int -> Int -> Int
minus m 0 = m
minus m n = minus m (n - 1) - 1

power :: Int -> Int -> Int
power m 1 = m
power m n = power m (n - 1) * m
