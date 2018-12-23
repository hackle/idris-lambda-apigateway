{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module: AWSLambda.Events.APIGateway
Description: Types for APIGateway Lambda requests and responses

Based on https://github.com/aws/aws-lambda-dotnet/tree/master/Libraries/src/Amazon.Lambda.APIGatewayEvents

To enable processing of API Gateway events, use the @events@ key in
@serverless.yml@ as usual:

> functions:
>   myapifunc:
>     handler: mypackage.mypackage-exe
>     events:
>       - http:
>           path: hello/{name}
>           method: get

Then use 'apiGatewayMain' in the handler to process the requests.
-}
module Types

import Decode
import Language.JSON
import Language.JSON.Data
-- import Lightyear.Core
import Http
import Http.Request

record RequestIdentity where
  constructor MkRequestIdentity
  riCognitoIdentityPoolId         : Maybe String
  riAccountId                     : Maybe String
  riCognitoIdentityId             : Maybe String
  riCaller                        : Maybe String
  riApiKey                        : Maybe String
  -- riSourceIp                      : Maybe String
  riCognitoAuthenticationType     : Maybe String
  riCognitoAuthenticationProvider : Maybe String
  riUserArn                       : Maybe String
  riUserAgent                     : Maybe String
  riUser                          : Maybe String

-- readParse : Decoder a => String -> Parser a
-- readParse str =
--   case readMaybe str of
--     Just result => pure result
--     Nothing     => fail $ "Failed to parse an " ++ msg

decodeRequestIdentity : Decoder RequestIdentity
decodeRequestIdentity jobj@(JObject obj) =
      Right MkRequestIdentity <*>
        decodeJSON (field "cognitoIdentityPoolId" (Decode.maybe string)) jobj <*>
        decodeJSON (field "accountId" (Decode.maybe string)) jobj <*>
        decodeJSON (field "cognitoIdentityId" (Decode.maybe string)) jobj <*>
        decodeJSON (field "caller" (Decode.maybe string)) jobj <*>
        decodeJSON (field "apiKey" (Decode.maybe string)) jobj <*>
        -- (decodeJSON (field (maybe string))  "sourceIp" >>= traverse (readParse "IP address")) obj <*>
        decodeJSON (field "cognitoAuthenticationType" (Decode.maybe string)) jobj <*>
        decodeJSON (field "cognitoAuthenticationProvider" (Decode.maybe string)) jobj <*>
        decodeJSON (field "userArn" (Decode.maybe string)) jobj <*>
        decodeJSON (field "userAgent" (Decode.maybe string)) jobj <*>
        decodeJSON (field "user" (Decode.maybe string)) jobj
decodeRequestIdentity json = error "RequestIdentity" json

-- data Authorizer = Authorizer
--   { _aPrincipalId : !(Maybe Text)
--   , _aClaims : !Object
--   , _aContext : !Object
--   } deriving (Eq, Show)
-- instance FromJSON Authorizer where
--   parseJSON = withObject "Authorizer" $ \o ->
--     Authorizer
--       <$> o .:? "principalId"
--       <*> o .:? "claims" .!= mempty
--       <*> (pure $ HashMap.delete "principalId" $ HashMap.delete "claims" o)
-- $(makeLenses ''Authorizer)

record ProxyRequestContext where
  constructor MkProxyRequestContext
  prcPath         : Maybe String
  prcAccountId    : String
  prcResourceId   : String
  prcStage        : String
  prcRequestId    : String
  prcIdentity     : RequestIdentity
  prcResourcePath : String
  prcHttpMethod   : String
  prcApiId        : String
  prcProtocol     : String
  -- prcAuthorizer   : Maybe Authorizer

decodeProxyRequestContext : Decoder ProxyRequestContext

record APIGatewayProxyRequest body where
  constructor MkAPIGatewayProxyRequest
  agprqResource              : String
  agprqPath                  : String
  agprqHttpMethod            : String
  agprqHeaders               : SortedMap String String
  agprqQueryStringParameters : SortedMap String String
  agprqPathParameters        : SortedMap String String
  agprqStageVariables        : SortedMap String String
  agprqRequestContext        : ProxyRequestContext
  agprqBody                  : Maybe body

decodeAPIGatewayProxyRequest : Decoder (APIGatewayProxyRequest String)
decodeAPIGatewayProxyRequest obj@(JObject _) =
    MkAPIGatewayProxyRequest
      <$> decodeJSON (field "resource" string) obj
      <*> decodeJSON (field "path" string) obj
      <*> decodeJSON (field "httpMethod" string) obj
      <*> (SortedMap.fromList <$> (decodeJSON (field "headers" (keyValuePairs string)) obj))
      <*> (SortedMap.fromList <$> (decodeJSON (field "queryStringParameters" (keyValuePairs string)) obj))
      <*> (SortedMap.fromList <$> (decodeJSON (field "pathParameters" (keyValuePairs string)) obj))
      <*> (SortedMap.fromList <$> (decodeJSON (field "stageVariables" (keyValuePairs string)) obj))
      <*> decodeJSON (field "requestContext" decodeProxyRequestContext) obj
      <*> decodeJSON (field "body" (Decode.maybe string)) obj
decodeAPIGatewayProxyRequest json = error "APIGatewayProxyRequest" json
    -- where
    --   -- Explicit type signatures so that we don't accidentally tell Aeson
    --   -- to try to parse the wrong sort of structure
    --   fromAWSHeaders : HashMap HeaderName HeaderValue -> HTTP.RequestHeaders
    --   fromAWSHeaders = fmap toHeader . HashMap.toList
    --     where
    --       toHeader = bimap (CI.mk . encodeUtf8) encodeUtf8
    --   fromAWSQuery : HashMap QueryParamName QueryParamValue -> HTTP.Query
    --   fromAWSQuery = fmap toQueryItem . HashMap.toList
    --     where
    --       toQueryItem = bimap encodeUtf8 (\x -> if Text.null x then Nothing else Just . encodeUtf8 $ x)

-- $(makeLenses ''APIGatewayProxyRequest)

-- | Get the request body, if there is one
requestBody : APIGatewayProxyRequest body -> Maybe body
requestBody = agprqBody

-- | Get the embedded request body, if there is one
-- requestBodyEmbedded : Getter (APIGatewayProxyRequest (Embedded v)) (Maybe v)
-- requestBodyEmbedded = requestBody . mapping unEmbed

-- | Get the binary (decoded Base64) request body, if there is one
-- requestBodyBinary : Getter (APIGatewayProxyRequest Base64) (Maybe ByteString)
-- requestBodyBinary = requestBody . mapping _Base64

record APIGatewayProxyResponse body where
  constructor MkAPIGatewayProxyResponse
  agprsStatusCode : Int
  agprsHeaders    : SortedMap String String
  agprsBody       : Maybe body

encodeMap : SortedMap String String -> JSON

encodeAPIGatewayProxyResponse : APIGatewayProxyResponse String -> JSON
encodeAPIGatewayProxyResponse resp =
  JObject [
    ("statusCode", JNumber $ cast $ agprsStatusCode resp)
    ,("headers", encodeMap $ agprsHeaders resp)
    ,("body", Maybe.fromMaybe JNull (Functor.map JString $ agprsBody resp))
  ]
--
-- instance FromText body => FromJSON (APIGatewayProxyResponse body) where
--   parseJSON =
--     withObject "APIGatewayProxyResponse" $ \o ->
--       APIGatewayProxyResponse <$> o .: "statusCode" <*>
--       (fromAWSHeaders <$> o .: "headers") <*>
--       o .:? "body"
--       -- Explicit type signatures so that we don't accidentally tell Aeson
--       -- to try to parse the wrong sort of structure
--     where
--       fromAWSHeaders : HashMap HeaderName HeaderValue -> HTTP.RequestHeaders
--       fromAWSHeaders = fmap toHeader . HashMap.toList
--         where
--           toHeader = bimap (CI.mk . encodeUtf8) encodeUtf8

-- $(makeLenses ''APIGatewayProxyResponse)

response : Int -> APIGatewayProxyResponse body
response statusCode = MkAPIGatewayProxyResponse statusCode empty Nothing

responseOK : APIGatewayProxyResponse body
responseOK = response 200

responseNotFound : APIGatewayProxyResponse body
responseNotFound = response 404

responseBadRequest : APIGatewayProxyResponse body
responseBadRequest = response 400

-- responseBody : Setter' (APIGatewayProxyResponse body) (Maybe body)
-- responseBody = agprsBody . at () . mapping unTextValue

-- responseBodyEmbedded : Setter' (APIGatewayProxyResponse (Embedded body)) (Maybe body)
-- responseBodyEmbedded = responseBody . mapping unEmbed
--
-- responseBodyBinary : Setter' (APIGatewayProxyResponse Base64) (Maybe ByteString)
-- responseBodyBinary = responseBody . mapping _Base64

{-| Process incoming events from @serverless-haskell@ using a provided function.

This is a specialisation of 'lambdaMain' for API Gateway.

The handler receives the input event given to the AWS Lambda function, and
its return value is returned from the function.

This is intended to be used as @main@, for example:

> import AWSLambda.Events.APIGateway
> import Control.Lens
> import Data.Aeson
> import Data.Aeson.Embedded
>
> main = apiGatewayMain handler
>
> handler : APIGatewayProxyRequest (Embedded Value) -> IO (APIGatewayProxyResponse (Embedded [Int]))
> handler request = do
>   putStrLn "This should go to logs"
>   print $ request ^. requestBody
>   pure $ responseOK & responseBodyEmbedded ?~ [1, 2, 3]

The type parameters @reqBody@ and @resBody@ represent the types of request and response body, respectively.
The @FromText@ and @ToText@ contraints are required because these values come from string fields
in the request and response JSON objects.
To get direct access to the body string, use @Text@ as the parameter type.
To treat the body as a stringified embedded JSON value, use @Embedded a@, where @a@ has the
appropriate @FromJSON@ or @ToJSON@ instances.
To treat the body as base 64 encoded binary use @Base64@.
-}
apiGatewayMain :
  (APIGatewayProxyRequest reqBody -> IO (APIGatewayProxyResponse resBody)) -- ^ Function to process the event
  -> IO ()
-- apiGatewayMain = lambdaMain
