tupleToStream :: ([Int], [Int], [Int]) -> [(Int, Int, Int)]
tupleToStream (x:xs, y:ys, z:zs) = (x, y, z) : tupleToStream (xs, ys, zs)


streamToTuple :: [(Int, Int, Int)] -> ([Int], [Int], [Int])
streamToTuple ((x1, x2, x3) : xs) = (x1:xs1, x2:xs2, x3:xs3)
    where
        (xs1, xs2, xs3) = streamToTuple xs


braidStreamHelper :: Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]
braidStreamHelper n ((x1, x2, x3) : )
    | n `mod` 2 == 0 = (x1, x2, x3) : braidStreamHelper (n + 1) xs
    | otherwise = (x1, x2, x3) : braidStreamHelper (n + 1) xs


braidStream :: ([Int], [Int], [Int]) -> ([Int], [Int], [Int])
braidStream inpt = outpt
    where
        tups = tupleToStream inpt
        outpt = streamToTuple (braidStreamHelper 0 tups)


-- solution review

