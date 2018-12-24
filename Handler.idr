module Handler

import Types

%access export

handler : APIGatewayProxyRequest String -> IO (APIGatewayProxyResponse String)
handler f = ?apiGatewayMain_rhs
