module Handler

import Types
import Data.SortedMap

%access export

handler : APIGatewayProxyRequest String -> IO (APIGatewayProxyResponse String)
handler _ = do
  putStrLn "trying to handle"
  pure $ MkAPIGatewayProxyResponse 200 empty $ Just "bro"
