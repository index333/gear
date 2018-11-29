module Round where
roundN :: Int -> Double -> Double
roundN n d = fromIntegral (round (d * 10 ^ n)) / fromIntegral (10 ^ n)
round1,round2 :: Double -> Double
round1 = roundN 1
round2 = roundN 2