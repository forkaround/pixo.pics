module Data.PaddleSignature exposing (PaddleSignature, parse, signatures, timestamp)


type PaddleSignature
    = PaddleSignature { timestamp : Int, signatures : List String }


parse : String -> Maybe PaddleSignature
parse header =
    let
        pairs =
            String.split ";" header
                |> List.filterMap
                    (\part ->
                        case String.split "=" (String.trim part) of
                            key :: rest ->
                                Just ( String.trim key, String.trim (String.join "=" rest) )

                            [] ->
                                Nothing
                    )

        stamps =
            List.filterMap
                (\( key, value ) ->
                    if key == "ts" && not (String.isEmpty value) && String.all Char.isDigit value then
                        String.toInt value

                    else
                        Nothing
                )
                pairs

        hashes =
            List.filterMap
                (\( key, value ) ->
                    if key == "h1" then
                        hex64 (String.toLower value)

                    else
                        Nothing
                )
                pairs
    in
    case ( stamps, hashes ) of
        ( first :: _, _ :: _ ) ->
            Just (PaddleSignature { timestamp = first, signatures = hashes })

        _ ->
            Nothing


hex64 : String -> Maybe String
hex64 value =
    if String.length value == 64 && String.all isLowerHex value then
        Just value

    else
        Nothing


isLowerHex : Char -> Bool
isLowerHex c =
    Char.isDigit c || (c >= 'a' && c <= 'f')


timestamp : PaddleSignature -> Int
timestamp (PaddleSignature signature) =
    signature.timestamp


signatures : PaddleSignature -> List String
signatures (PaddleSignature signature) =
    signature.signatures
