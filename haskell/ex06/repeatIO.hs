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
