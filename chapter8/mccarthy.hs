mc91 :: Integer -> Integer
mc91 n
  | n > 100 = n - 10
  | n <= 100 = mc91 (mc91 (n + 11))