import Utils

-- n = 10001 for problem #10

getPrime :: Int -> Int

getPrime n = head . drop (n - 1) . filter prime $ [2..]