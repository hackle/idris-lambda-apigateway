import Decode

import Language.JSON
import Types

main : IO ()
main = do
  Right content <- readFile "sampleApiRequest.txt" | Left _ => putStrLn "Cannot open file"
  case decodeString decodeAPIGatewayProxyRequest content of
    Left err => putStrLn err
    Right request => putStrLn $ "success"
