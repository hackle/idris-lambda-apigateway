module Rest

import Http
import Http.Request
import Http.Uri

public export
record EndPoint where
  constructor MkEndPoint
  epHost: String
  epPort: Int
  epPath: String

export
post : (endpoint: EndPoint) ->
        (body: String) ->
        (headers: SortedMap String String) ->
        IO (Either HttpError (Response String))
post (MkEndPoint host port path) body headers =
  httpRequest $ MkRequest POST uri body headers where
      auth : URIAuth
      auth = MkURIAuth Nothing Nothing host port
      uri : URI
      uri = MkURI "http" auth path [] ""

export
get : (endpoint: EndPoint) ->
        IO (Either HttpError (Response String))
get (MkEndPoint host port path) = simpleHttp host port path
