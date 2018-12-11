import System

printOne : (String, String) -> IO ()
<<<<<<< HEAD
printOne (k, v) = do
  putStrLn $ k ++ ": " ++ v
=======
printOne (a, b) = putStrLn $ a ++ ": " ++ b
>>>>>>> e517079f79684708f759a29daaa97af0abb5d35a

main : IO ()
main = do
  envs <- System.getEnvironment
<<<<<<< HEAD
  putStrLn "===ENVS==="
  sequence_ $ map printOne envs
  putStrLn "===END OF ENVS==="
=======
  sequence_ $ map printOne envs
>>>>>>> e517079f79684708f759a29daaa97af0abb5d35a
