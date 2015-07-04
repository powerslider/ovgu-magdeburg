historyIO :: IO [String]
historyIO = do xs <- getLine
               if xs == ":show"
               then accumulateIO xs


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

repeatIO :: IO Bool -> IO () -> IO ()
repeatIO test oper = do pred <- test
                        oper
                        if pred == False
                        then return ()
                        else
                            do result <- repeatIO test oper
                               return result

test = do input <- getLine
          if input == "q" || input == "quit"
          then return False
          else
              return True

-- *Main> repeatIO test (putStrLn "next")
-- a
-- next
-- b
-- next
-- c
-- next
-- d
-- next
-- q
-- next
