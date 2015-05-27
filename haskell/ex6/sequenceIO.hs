accumulateIO :: [IO a] -> IO [a]
accumulateIO [] = do return []

accumulateIO (x:xs) = do listHead <- x
                         listTail <- accumulateIO xs
                         return (listHead:listTail)

-- *Main> accumulateIO [getLine, getLine]
-- has
-- kell
-- ["has","kell"]


sequenceIO :: [IO a] -> IO ()
sequenceIO [] = do return ()

sequenceIO (x:xs) = do listHead <- x
                       sequenceIO xs

-- *Main> sequenceIO [putStrLn "has", putStrLn "kell"]
-- has
-- kell


seqList :: [a -> IO a] -> a -> IO a
seqList [] arg = do return arg

seqList (x:xs) arg = do listHead <- x arg
                        seqList xs listHead

addKell x = do return (x ++ "kell")

-- *Main> seqList [addKell, addKell, addKell] "has"
-- "haskellkellkell"
