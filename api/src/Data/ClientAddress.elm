module Data.ClientAddress exposing (ClientAddress, fromHeaders, toString)

import Dict exposing (Dict)


type ClientAddress
    = ClientAddress String


fromHeaders : Dict String String -> ClientAddress
fromHeaders headers =
    ClientAddress
        (String.left 64
            (case Dict.get "x-real-ip" headers of
                Just address ->
                    String.trim address

                Nothing ->
                    Dict.get "x-forwarded-for" headers
                        |> Maybe.andThen (String.split "," >> List.head)
                        |> Maybe.map String.trim
                        |> Maybe.withDefault "unknown"
            )
        )


toString : ClientAddress -> String
toString (ClientAddress value) =
    value
