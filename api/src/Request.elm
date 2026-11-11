module Request exposing (Request, clientAddress, header, linkToken)

import Data.ClientAddress as ClientAddress exposing (ClientAddress)
import Data.LinkToken as LinkToken exposing (LinkToken)
import Dict exposing (Dict)


type alias Request =
    { method : String
    , path : List String
    , query : Dict String String
    , headers : Dict String String
    , body : String
    }


header : String -> Request -> Maybe String
header name request =
    Dict.get name request.headers


linkToken : Request -> Maybe LinkToken
linkToken request =
    header "authorization" request
        |> Maybe.andThen
            (\value ->
                if String.toLower (String.left 7 value) == "bearer " then
                    Just (String.trim (String.dropLeft 7 value))

                else
                    Nothing
            )
        |> Maybe.andThen LinkToken.fromString


clientAddress : Request -> ClientAddress
clientAddress request =
    ClientAddress.fromHeaders request.headers
