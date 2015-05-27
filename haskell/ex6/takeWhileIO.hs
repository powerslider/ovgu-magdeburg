takeWhileIO :: (a -> Bool) -> IO a -> IO [a]
takeWhileIO pred oper = do listHead <- oper
                           let predHead = pred listHead
                           if predHead == False
                           then return []
                           else
                               do listTail <- takeWhileIO pred oper
                                  return (listHead:listTail)


-- *Main> takeWhileIO (\x -> if x == "quit" then False else True) getLine
-- I
-- love 
-- Haskell
-- !!!
-- quit
-- ["I","love","Haskell","!!!"]
