fmapIO :: (a -> b) -> IO a -> IO b
fmapIO f action = do result <- action
                     return (f result)

fmapIO' :: (a -> b) -> IO a -> IO b
fmapIO' f action = 
        action >>= (\result -> return (f result))

-- *Main> fmapIO (\x -> reverse x) getLine
-- haskell
-- "lleksah"
-- *Main> fmapIO' (\x -> reverse x) getLine
-- haskell
-- "lleksah"
