module Main

import System
import ProcessHelper
import Rest
import Http.Error
import Http.Response

record Context where
  constructor MkContext
  runTimeApi: string

runTimeApiKey : String
runTimeApiKey = "AWS_LAMBDA_RUNTIME_API"

executeWith : (responseBody : String) -> IO ()
executeWith responseBody = do
  response <- execAndReadOutput $ "echo \"" ++ responseBody ++ "\""
  putStrLn response

main : IO ()
main = do
  envs <- System.getEnvironment
  case find (\(k, _) => k == runTimeApiKey) envs of
    Nothing => putStrLn "Cannot find api root"
    (Just (_, v)) =>
      let [host, port] = split (== ':') v in do
          putStrLn $ "host is " ++ host ++ " port is " ++ port
          event <- get host (cast {to=Int} port) "2018-06-01/runtime/invocation/next"
          case event of
            (Left l) => putStrLn $ "Error: " ++ (show l)
            (Right (MkResponse responseStatus responseHeaders responseBody)) => executeWith responseBody
          -- response <-
          -- putStrLn response
