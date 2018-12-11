import System

printOne : (String, String) -> IO ()
printOne (k, v) = do
  putStrLn $ k ++ ": " ++ v

main : IO ()
main = do
  envs <- System.getEnvironment
  putStrLn "===ENVS==="
  sequence_ $ map printOne envs
  putStrLn "===END OF ENVS==="
