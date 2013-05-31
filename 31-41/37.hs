--37.hs Euler's totient function phi(m) improved

phi:: [(Double, Double)] -> Double
phi [] = 1
phi ((p, m) : xs) = (p-1) * p ** (m-1) * (phi xs)