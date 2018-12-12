module Rest

import Http

export
get : (host: String) -> (port: Int) -> (path: String) -> IO (Either HttpError (Response String))
get host port path = simpleHttp host port path
