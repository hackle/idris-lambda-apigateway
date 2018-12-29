module Handler

import Types
import Data.SortedMap

%access export

handler : APIGatewayProxyRequest String -> IO (APIGatewayProxyResponse String)
handler req = do
  pure $ MkAPIGatewayProxyResponse 200 headers $ Just $ "<html><body><h1>" ++ path ++ "</h1><p>" ++ urlBase ++ "</p></body></html>"
where
    headers: SortedMap String String
    headers = SortedMap.fromList [ ("content-type", "text/html") ]
    path: String
    path = fromMaybe "not found" $ agprqPathParameters req >>= SortedMap.lookup "name"
    urlBase: String
    urlBase = fromMaybe "/" $ agprqStageVariables req >>= SortedMap.lookup "url_base"
