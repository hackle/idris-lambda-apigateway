import System

printOne : (String, String) -> IO ()
printOne (a, b) = putStrLn $ a ++ ": " ++ b

main : IO ()
main = do
  envs <- System.getEnvironment
  sequence_ $ map printOne envs
