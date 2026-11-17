module SigV4Test exposing (suite)

import Expect
import SigV4
import Test exposing (Test, describe, test)
import Time


credentials : SigV4.Credentials
credentials =
    { accessKeyId = "AKIAIOSFODNN7EXAMPLE"
    , secretAccessKey = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
    , region = "us-east-1"
    , service = "s3"
    }


r2 : SigV4.Credentials
r2 =
    { credentials | region = "auto" }


time : Time.Posix
time =
    Time.millisToPosix 1369353600000


getObject : SigV4.Unsigned
getObject =
    { method = "GET"
    , host = "examplebucket.s3.amazonaws.com"
    , path = [ "test.txt" ]
    , query = []
    , headers = [ ( "range", "bytes=0-9" ) ]
    , payloadHash = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    }


header : String -> List ( String, String ) -> Maybe String
header name headers =
    headers
        |> List.filter (\( found, _ ) -> found == name)
        |> List.head
        |> Maybe.map Tuple.second


isHex : String -> Bool
isHex text =
    String.all Char.isHexDigit text && text == String.toLower text


suite : Test
suite =
    describe "SigV4"
        [ test "amzDate writes the UTC time as YYYYMMDDTHHMMSSZ" <|
            \() ->
                SigV4.amzDate time
                    |> Expect.equal "20130524T000000Z"
        , test "signs AWS's GET Object example" <|
            \() ->
                header "authorization" (SigV4.sign credentials time getObject)
                    |> Expect.equal (Just "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request, SignedHeaders=host;range;x-amz-content-sha256;x-amz-date, Signature=f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41")
        , test "never returns a host header" <|
            \() ->
                SigV4.sign credentials time getObject
                    |> List.map Tuple.first
                    |> List.member "host"
                    |> Expect.equal False
        , test "encodes a path segment strictly in the canonical URI" <|
            \() ->
                SigV4.canonicalRequest time { getObject | path = [ "a b(1)*.jpg" ] }
                    |> String.split "\n"
                    |> List.drop 1
                    |> List.head
                    |> Expect.equal (Just "/a%20b%281%29%2A.jpg")
        , test "signs an R2-shaped PUT with its extra header" <|
            \() ->
                header "authorization"
                    (SigV4.sign r2
                        time
                        { method = "PUT"
                        , host = "account.r2.cloudflarestorage.com"
                        , path = [ "bucket", "a.jpg" ]
                        , query = []
                        , headers = [ ( "cache-control", "public, max-age=3600" ) ]
                        , payloadHash = SigV4.unsignedPayload
                        }
                    )
                    |> Maybe.map (String.startsWith "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/auto/s3/aws4_request, SignedHeaders=cache-control;host;x-amz-content-sha256;x-amz-date, Signature=")
                    |> Expect.equal (Just True)
        , test "presigns AWS's query authorization example" <|
            \() ->
                SigV4.presign credentials
                    time
                    { method = "GET"
                    , host = "examplebucket.s3.amazonaws.com"
                    , path = [ "test.txt" ]
                    , headers = []
                    , expires = 86400
                    }
                    |> Expect.equal "https://examplebucket.s3.amazonaws.com/test.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAIOSFODNN7EXAMPLE%2F20130524%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20130524T000000Z&X-Amz-Expires=86400&X-Amz-SignedHeaders=host&X-Amz-Signature=aeeed9bbccd4d02ee5c0109b86d86835f995330da4c265957d157751f604d404"
        , test "presigns an R2-shaped PUT with its content type signed" <|
            \() ->
                let
                    url =
                        SigV4.presign r2
                            time
                            { method = "PUT"
                            , host = "account.r2.cloudflarestorage.com"
                            , path = [ "bucket", "a.jpg" ]
                            , headers = [ ( "content-type", "image/jpeg" ) ]
                            , expires = 600
                            }
                in
                ( String.contains "X-Amz-SignedHeaders=content-type%3Bhost" url
                , String.contains "X-Amz-Expires=600" url
                , case String.split "&X-Amz-Signature=" url of
                    [ _, hex ] ->
                        String.length hex == 64 && isHex hex

                    _ ->
                        False
                )
                    |> Expect.equal ( True, True, True )
        ]
