module Utils where


mean :: (Fractional a, Foldable t) => t a -> a
mean els = (/n) $ sum els
  where n = fromIntegral $ length els
