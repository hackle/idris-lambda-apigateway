module Handler

import Types
import Contents
import Data.SortedMap
import Language.JSON

%access export

handler : APIGatewayProxyRequest String -> IO (APIGatewayProxyResponse String)
handler req = pure $ MkAPIGatewayProxyResponse 200 headers $ Just !mkResponse
where
    headers: SortedMap String String
    headers = SortedMap.fromList [ ("content-type", "text/markdown") ]
    slug : String
    slug =
      let name = agprqPathParameters req >>= SortedMap.lookup "name" in
          fromMaybe "" name
    mkResponse : IO String
    mkResponse =
      let post = findPostBySlug slug in
          pure $ format 4 $ encodeBlogResponse $ MkBlogResponse (title post) !(getContent post) contents
