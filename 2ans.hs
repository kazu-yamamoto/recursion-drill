plus :: Int -> Int -> Int
plus m n = plus' m n m

plus' :: Int -> Int -> Int -> Int
plus' _ 0 acc = acc
plus' m n acc = plus' m (n - 1) (acc + 1)

minus :: Int -> Int -> Int
minus m n = minus' m n m

minus' :: Int -> Int -> Int -> Int
minus' _ 0 acc = acc
minus' m n acc = minus' m (n - 1) (acc - 1)

power :: Int -> Int -> Int
power m n = power' m n m

power' :: Int -> Int -> Int -> Int
power' _ 1 acc = acc
power' m n acc = power' m (n - 1) (acc * m)
