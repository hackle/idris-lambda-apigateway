import Decode

import Language.JSON

||| Decode a JString
string1 : Decoder String
string1 (JString str) = Right str
string1 json = Left "string"
