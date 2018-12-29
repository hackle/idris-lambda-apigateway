module Types

import Decode
import Language.JSON
import Language.JSON.Data
import Data.SortedMap

%access export

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

decodeRequestIdentity : Decoder RequestIdentity
decodeRequestIdentity jobj@(JObject obj) =
      Right MkRequestIdentity <*>
        decodeJSON (Decode.maybe (field "cognitoIdentityPoolId" string)) jobj <*>
        decodeJSON (Decode.maybe (field "accountId" string)) jobj <*>
        decodeJSON (Decode.maybe (field "cognitoIdentityId" string)) jobj <*>
        decodeJSON (Decode.maybe (field "caller" string)) jobj <*>
        decodeJSON (Decode.maybe (field "apiKey" string)) jobj <*>
        -- (decodeJSON (field (maybe string))  "sourceIp" >>= traverse (readParse "IP address")) obj <*>
        decodeJSON (Decode.maybe (field "cognitoAuthenticationType" string)) jobj <*>
        decodeJSON (Decode.maybe (field "cognitoAuthenticationProvider" string)) jobj <*>
        decodeJSON (Decode.maybe (field "userArn" string)) jobj <*>
        decodeJSON (Decode.maybe (field "userAgent" string)) jobj <*>
        decodeJSON (Decode.maybe (field "user" string)) jobj
decodeRequestIdentity json = error "RequestIdentity" json

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
decodeProxyRequestContext obj@(JObject _) =
  MkProxyRequestContext
  <$> decodeJSON (Decode.maybe (field "path" string)) obj
  <*> decodeJSON (field "accountId" string) obj
  <*> decodeJSON (field "resourceId" string) obj
  <*> decodeJSON (field "stage" string) obj
  <*> decodeJSON (field "requestId" string) obj
  <*> decodeJSON (field "identity" decodeRequestIdentity) obj
  <*> decodeJSON (field "resourcePath" string) obj
  <*> decodeJSON (field "httpMethod" string) obj
  <*> decodeJSON (field "apiId" string) obj
  <*> decodeJSON (field "protocol" string) obj
decodeProxyRequestContext json = error "ProxyRequestContext" json

public export
record APIGatewayProxyRequest body where
  constructor MkAPIGatewayProxyRequest
  agprqResource              : String
  agprqPath                  : String
  agprqHttpMethod            : String
  agprqHeaders               : Maybe (SortedMap String String)
  agprqQueryStringParameters : Maybe (SortedMap String String)
  agprqPathParameters        : Maybe (SortedMap String String)
  agprqStageVariables        : Maybe (SortedMap String String)
  agprqRequestContext        : ProxyRequestContext
  agprqBody                  : Maybe body

decodeAPIGatewayProxyRequest : Decoder (APIGatewayProxyRequest String)
decodeAPIGatewayProxyRequest obj@(JObject _) =
    MkAPIGatewayProxyRequest
      <$> decodeJSON (field "resource" string) obj
      <*> decodeJSON (field "path" string) obj
      <*> decodeJSON (field "httpMethod" string) obj
      <*> maybeMap obj "headers"
      <*> maybeMap obj "queryStringParameters"
      <*> maybeMap obj "pathParameters"
      <*> maybeMap obj "stageVariables"
      <*> decodeJSON (field "requestContext" decodeProxyRequestContext) obj
      <*> decodeJSON (Decode.maybe (field "body" string)) obj
      where
        maybeMap : JSON -> String -> Either String (Maybe (SortedMap String String))
        maybeMap obj fieldName =
          let decoded = decodeJSON (Decode.maybe (field fieldName (keyValuePairs string))) obj in
              (SortedMap.fromList <$>) <$> decoded
decodeAPIGatewayProxyRequest json = error "APIGatewayProxyRequest" json

requestBody : APIGatewayProxyRequest body -> Maybe body
requestBody = agprqBody

record APIGatewayProxyResponse body where
  constructor MkAPIGatewayProxyResponse
  agprsStatusCode : Int
  agprsHeaders    : SortedMap String String
  agprsBody       : Maybe body

encodeMap : SortedMap String String -> JSON
encodeMap m = JObject $ mapOne <$> toList m where
  mapOne (k, v) = (k, JString v)

encodeAPIGatewayProxyResponse : APIGatewayProxyResponse String -> JSON
encodeAPIGatewayProxyResponse resp =
  JObject [
    ("statusCode", JNumber $ cast $ agprsStatusCode resp)
    ,("headers", encodeMap $ agprsHeaders resp)
    ,("body", Maybe.fromMaybe JNull (JString <$> agprsBody resp))
  ]

response : Int -> APIGatewayProxyResponse body
response statusCode = MkAPIGatewayProxyResponse statusCode empty Nothing

responseOK : APIGatewayProxyResponse body
responseOK = response 200

responseNotFound : APIGatewayProxyResponse body
responseNotFound = response 404

responseBadRequest : APIGatewayProxyResponse body
responseBadRequest = response 400
