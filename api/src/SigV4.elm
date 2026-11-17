module SigV4 exposing
    ( Credentials
    , Unsigned
    , amzDate
    , canonicalRequest
    , presign
    , sign
    , unsignedPayload
    )

import Hmac
import SHA256
import Time
import Url


type alias Credentials =
    { accessKeyId : String
    , secretAccessKey : String
    , region : String
    , service : String
    }


type alias Unsigned =
    { method : String
    , host : String
    , path : List String
    , query : List ( String, String )
    , headers : List ( String, String )
    , payloadHash : String
    }


unsignedPayload : String
unsignedPayload =
    "UNSIGNED-PAYLOAD"


amzDate : Time.Posix -> String
amzDate time =
    let
        part width value =
            String.padLeft width '0' (String.fromInt value)
    in
    part 4 (Time.toYear Time.utc time)
        ++ part 2 (monthNumber (Time.toMonth Time.utc time))
        ++ part 2 (Time.toDay Time.utc time)
        ++ "T"
        ++ part 2 (Time.toHour Time.utc time)
        ++ part 2 (Time.toMinute Time.utc time)
        ++ part 2 (Time.toSecond Time.utc time)
        ++ "Z"


monthNumber : Time.Month -> Int
monthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


encode : String -> String
encode text =
    Url.percentEncode text
        |> String.replace "!" "%21"
        |> String.replace "'" "%27"
        |> String.replace "(" "%28"
        |> String.replace ")" "%29"
        |> String.replace "*" "%2A"


canonicalUri : List String -> String
canonicalUri path =
    "/" ++ String.join "/" (List.map encode path)


canonicalQuery : List ( String, String ) -> String
canonicalQuery query =
    query
        |> List.map (\( name, value ) -> ( encode name, encode value ))
        |> List.sort
        |> List.map (\( name, value ) -> name ++ "=" ++ value)
        |> String.join "&"


normalizedHeaders : List ( String, String ) -> List ( String, String )
normalizedHeaders headers =
    headers
        |> List.map (\( name, value ) -> ( String.toLower name, String.trim value ))
        |> List.sortBy Tuple.first


canonicalHeaders : List ( String, String ) -> String
canonicalHeaders headers =
    headers
        |> List.map (\( name, value ) -> name ++ ":" ++ value ++ "\n")
        |> String.concat


signedHeaderNames : List ( String, String ) -> String
signedHeaderNames headers =
    headers
        |> List.map Tuple.first
        |> String.join ";"


signedHeaders : Time.Posix -> Unsigned -> List ( String, String )
signedHeaders time unsigned =
    normalizedHeaders
        ([ ( "host", unsigned.host )
         , ( "x-amz-content-sha256", unsigned.payloadHash )
         , ( "x-amz-date", amzDate time )
         ]
            ++ unsigned.headers
        )


canonicalRequest : Time.Posix -> Unsigned -> String
canonicalRequest time unsigned =
    let
        headers =
            signedHeaders time unsigned
    in
    String.join "\n"
        [ unsigned.method
        , canonicalUri unsigned.path
        , canonicalQuery unsigned.query
        , canonicalHeaders headers
        , signedHeaderNames headers
        , unsigned.payloadHash
        ]


dateStamp : Time.Posix -> String
dateStamp time =
    String.left 8 (amzDate time)


scope : Credentials -> Time.Posix -> String
scope credentials time =
    String.join "/" [ dateStamp time, credentials.region, credentials.service, "aws4_request" ]


stringToSign : Credentials -> Time.Posix -> String -> String
stringToSign credentials time request =
    String.join "\n"
        [ "AWS4-HMAC-SHA256"
        , amzDate time
        , scope credentials time
        , SHA256.toHex (SHA256.fromString request)
        ]


signature : Credentials -> Time.Posix -> String -> String
signature credentials time request =
    let
        step key message =
            Hmac.sha256 key (Hmac.utf8 message)

        signingKey =
            step (Hmac.utf8 ("AWS4" ++ credentials.secretAccessKey)) (dateStamp time)
                |> (\kDate -> step kDate credentials.region)
                |> (\kRegion -> step kRegion credentials.service)
                |> (\kService -> step kService "aws4_request")
    in
    Hmac.toHex (step signingKey (stringToSign credentials time request))


sign : Credentials -> Time.Posix -> Unsigned -> List ( String, String )
sign credentials time unsigned =
    [ ( "authorization"
      , "AWS4-HMAC-SHA256 Credential="
            ++ credentials.accessKeyId
            ++ "/"
            ++ scope credentials time
            ++ ", SignedHeaders="
            ++ signedHeaderNames (signedHeaders time unsigned)
            ++ ", Signature="
            ++ signature credentials time (canonicalRequest time unsigned)
      )
    , ( "x-amz-content-sha256", unsigned.payloadHash )
    , ( "x-amz-date", amzDate time )
    ]
        ++ unsigned.headers


presign :
    Credentials
    -> Time.Posix
    -> { method : String, host : String, path : List String, headers : List ( String, String ), expires : Int }
    -> String
presign credentials time request =
    let
        headers =
            normalizedHeaders (( "host", request.host ) :: request.headers)

        query =
            canonicalQuery
                [ ( "X-Amz-Algorithm", "AWS4-HMAC-SHA256" )
                , ( "X-Amz-Credential", credentials.accessKeyId ++ "/" ++ scope credentials time )
                , ( "X-Amz-Date", amzDate time )
                , ( "X-Amz-Expires", String.fromInt request.expires )
                , ( "X-Amz-SignedHeaders", signedHeaderNames headers )
                ]

        uri =
            canonicalUri request.path

        canonical =
            String.join "\n"
                [ request.method
                , uri
                , query
                , canonicalHeaders headers
                , signedHeaderNames headers
                , unsignedPayload
                ]
    in
    "https://" ++ request.host ++ uri ++ "?" ++ query ++ "&X-Amz-Signature=" ++ signature credentials time canonical
