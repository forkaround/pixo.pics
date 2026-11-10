module Origin exposing (Origin, isSecure, local, parse, rpId, toString)

import WebAuthn


type Origin
    = Origin
        { url : String
        , rpId : String
        , secure : Bool
        }


parse : String -> Result String Origin
parse url =
    case WebAuthn.rpId url of
        Just host ->
            Ok
                (Origin
                    { url = url
                    , rpId = host
                    , secure = String.startsWith "https://" url
                    }
                )

        Nothing ->
            Err ("Expected an origin like https://pixo.pics, got: " ++ url)


local : Origin
local =
    Origin
        { url = "http://localhost:5173"
        , rpId = "localhost"
        , secure = False
        }


toString : Origin -> String
toString (Origin origin) =
    origin.url


rpId : Origin -> String
rpId (Origin origin) =
    origin.rpId


isSecure : Origin -> Bool
isSecure (Origin origin) =
    origin.secure
