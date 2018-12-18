import Rest
import Http
import Http.Uri
import Http.Error
import Http.RawResponse
import Http.Request
import Http.Response
import System

processArgs : List String -> Maybe (Host, Port, String)
processArgs (_::host::port::path::_) =
  Just (host, (cast port), path)
processArgs (_::host::path::_) = Just (host, 80, path)
processArgs _ = Nothing

main : IO ()
main = do
  case processArgs !getArgs of
    Just (host, port, path) =>
      case !(post (MkEndPoint host port path) "{ \"foo\": \"bro\" }" $ fromList []) of
        Left err => print err >>= \_ => putStr "\n"
        Right res => do
          putStrLn "Response status:"
          print (responseStatus res)
          putStr "\n\n"

          putStrLn "Headers Received:"
          traverse (\x => do print x; putStr "\n") (Data.SortedMap.toList $ responseHeaders res)
          putStr "\n\n"

          putStrLn "Body:"
          print (responseBody res)
          putStr "\n\n"
    Nothing => putStrLn "Usage: ./simple host [port] path"
