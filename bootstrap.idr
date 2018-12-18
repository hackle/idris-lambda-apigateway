module Main

import System
import ProcessHelper
import Rest
import Http.Error
import Http.Response
import Http

record Context where
  constructor MkContext
  runTimeApi: string

data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO

runInfIO : InfIO -> IO ()
runInfIO (Do action cont) = do
  res <- action
  runInfIO (cont res)

runTimeApiKey : String
runTimeApiKey = "AWS_LAMBDA_RUNTIME_API"

escape : String -> String
escape x = concat $ intersperse "\\\"" $ split (== '"') x

executeWith : EndPoint -> (body : String) -> IO ()
executeWith endpoint body = do
  -- execAndReadOutput $ "curl -X POST \"http://" ++ host ++ ":" ++ show port ++ path ++ "\"  -d \"YO BRO\""
  -- -- post endpoint "blah" empty
  -- pure ()
  -- putStrLn $ "posting to " ++ (epPath endpoint)
  -- response <- execAndReadOutput $ "echo \"" ++ escape body ++ "\""
  case !(post endpoint body empty) of
    (Left (HttpSocketError x)) => putStrLn $ "Socket Error when responding: " ++ show x
    (Left (HttpParseError x)) => putStrLn $ "Parse Error when responding: " ++ show x
    (Right res1) => do
      putStrLn $ "Success with status: " ++ (responseStatusComment $ responseStatus res1)
      putStrLn $ "body: " ++ (responseBody res1)

findEnv : (key: String) -> (envs: List (String, String)) -> Maybe String
findEnv key envs = Just $ snd !(List.find (\(k, _) => k == key) envs)

printKvps : List (String, String) -> IO ()
printKvps kvps = do
  putStrLn $ "KVPs are " ++ (show $ length kvps) ++ " in length"
  putStrLn $ concat $ map (\(k, v) => k ++ ": " ++ v ++ "\r\n") kvps

printHeaders : SortedMap String String -> IO ()
printHeaders smap = printKvps $ toList smap

handleEvent : String -> Int -> Response String -> IO ()
handleEvent host port response = do
  case getHeader response "Lambda-Runtime-Aws-Request-Id" of
    Nothing => putStrLn "No request Id found"
    Just reqId => do
      let endpoint = (MkEndPoint host port $ "/2018-06-01/runtime/invocation/" ++ reqId ++ "/response")
      executeWith endpoint (responseBody response)

processEvent : String -> Int -> IO ()
processEvent host port = do
      event <- get $ MkEndPoint host port "/2018-06-01/runtime/invocation/next"
      case event of
        (Left (HttpSocketError x)) => putStrLn $ "Socker Error when getting event: " ++ show x
        (Left (HttpParseError x)) => putStrLn $ "Parse Error when getting event: " ++ show x
        (Right res) => do
          -- printHeaders $ responseHeaders res
          handleEvent host port res

runLoop : String -> Int -> Nat -> IO ()
runLoop _ _ Z = pure ()
runLoop host port (S n) = do
  processEvent host port
  putStrLn "sleeping now"
  usleep 500
  runLoop host port n

main : IO ()
main = do
  envs <- System.getEnvironment
  printKvps envs
  case findEnv runTimeApiKey envs of
    Nothing => putStrLn "Cannot find api root"
    (Just v) => do
          let [host, port] = split (== ':') v
          putStrLn $ "host is " ++ host ++ " port is " ++ port
          let iport = cast {to=Int} port
          runLoop host iport 20
