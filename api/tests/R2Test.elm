module R2Test exposing (suite)

import Data.PictureId as PictureId exposing (PictureId)
import Effect exposing (Effect)
import Expect exposing (Expectation)
import Http
import Services.R2 as R2
import Test exposing (Test, describe, test)
import Test.Http
import Time


none :
    { r2AccountId : Maybe String
    , r2AccessKeyId : Maybe String
    , r2SecretAccessKey : Maybe String
    , r2Bucket : Maybe String
    , r2UploadBucket : Maybe String
    , imagesOrigin : Maybe String
    }
none =
    { r2AccountId = Nothing
    , r2AccessKeyId = Nothing
    , r2SecretAccessKey = Nothing
    , r2Bucket = Nothing
    , r2UploadBucket = Nothing
    , imagesOrigin = Nothing
    }


all : { r2AccountId : Maybe String, r2AccessKeyId : Maybe String, r2SecretAccessKey : Maybe String, r2Bucket : Maybe String, r2UploadBucket : Maybe String, imagesOrigin : Maybe String }
all =
    { r2AccountId = Just "acc"
    , r2AccessKeyId = Just "key"
    , r2SecretAccessKey = Just "secret"
    , r2Bucket = Just "pictures"
    , r2UploadBucket = Just "uploads"
    , imagesOrigin = Just "https://img.pixo.pics"
    }


settings : R2.Config
settings =
    { accountId = "acc"
    , accessKeyId = "key"
    , secretAccessKey = "secret"
    , bucket = "pictures"
    , uploadBucket = "uploads"
    , publicOrigin = "https://img.pixo.pics"
    }


idText : String
idText =
    "6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11"


id : Maybe PictureId
id =
    PictureId.fromString idText


time : Time.Posix
time =
    Time.millisToPosix 0


problem : String
problem =
    "R2_ACCOUNT_ID, R2_ACCESS_KEY_ID, R2_SECRET_ACCESS_KEY, R2_BUCKET, R2_UPLOAD_BUCKET and IMAGES_ORIGIN must all be set, or none."


withId : (PictureId -> Expectation) -> Expectation
withId toExpectation =
    case id of
        Just found ->
            toExpectation found

        Nothing ->
            Expect.fail "The example id should parse."


withRequest : (PictureId -> Effect (Result String a)) -> (Effect.HttpRequest (Result String a) -> Expectation) -> Expectation
withRequest toEffect toExpectation =
    withId
        (\found ->
            case toEffect found of
                Effect.Http made ->
                    toExpectation made

                _ ->
                    Expect.fail "Expected an HTTP request."
        )


header : String -> List ( String, String ) -> Maybe String
header name headers =
    headers
        |> List.filter (\( found, _ ) -> found == name)
        |> List.head
        |> Maybe.map Tuple.second


response : Int -> List ( String, String ) -> String -> Http.Response String
response statusCode headers body =
    Test.Http.httpResponse { statusCode = statusCode, headers = headers, body = body }


suite : Test
suite =
    describe "R2"
        [ describe "config"
            [ test "is Nothing when no setting is given" <|
                \() ->
                    R2.config none
                        |> Expect.equal (Ok Nothing)
            , test "reads all six settings" <|
                \() ->
                    R2.config all
                        |> Expect.equal (Ok (Just settings))
            , test "refuses a partial set" <|
                \() ->
                    R2.config { all | r2Bucket = Nothing }
                        |> Expect.equal (Err problem)
            , test "refuses an origin that is not https or localhost" <|
                \() ->
                    R2.config { all | imagesOrigin = Just "img.pixo.pics" }
                        |> Expect.equal (Err problem)
            , test "accepts a localhost origin" <|
                \() ->
                    R2.config { all | imagesOrigin = Just "http://localhost:5173" }
                        |> Result.map (Maybe.map .publicOrigin)
                        |> Expect.equal (Ok (Just "http://localhost:5173"))
            , test "removes a trailing slash from the origin" <|
                \() ->
                    R2.config { all | imagesOrigin = Just "https://img.pixo.pics/" }
                        |> Result.map (Maybe.map .publicOrigin)
                        |> Expect.equal (Ok (Just "https://img.pixo.pics"))
            ]
        , describe "keys"
            [ test "names the public file and the staged one" <|
                \() ->
                    withId
                        (\found ->
                            ( R2.publicKey found, R2.stagedKey found, R2.publicUrl settings found )
                                |> Expect.equal
                                    ( idText ++ ".jpg"
                                    , "incoming/" ++ idText ++ ".jpg"
                                    , "https://img.pixo.pics/" ++ idText ++ ".jpg"
                                    )
                        )
            ]
        , describe "uploadLink"
            [ test "signs a 10-minute PUT of the staged key with its content type" <|
                \() ->
                    withId
                        (\found ->
                            R2.uploadLink settings time found
                                |> Expect.all
                                    [ .url
                                        >> String.startsWith
                                            ("https://acc.r2.cloudflarestorage.com/uploads/incoming/"
                                                ++ idText
                                                ++ ".jpg?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=key%2F19700101%2Fauto%2Fs3%2Faws4_request&X-Amz-Date=19700101T000000Z&X-Amz-Expires=600&X-Amz-SignedHeaders=content-type%3Bhost&X-Amz-Signature="
                                            )
                                        >> Expect.equal True
                                    , .headers >> Expect.equal [ ( "content-type", "image/jpeg" ) ]
                                    ]
                        )
            ]
        , describe "head"
            [ test "sends a signed HEAD for the staged key, without a host header" <|
                \() ->
                    withRequest (R2.head settings time)
                        (\made ->
                            Expect.all
                                [ \m -> Expect.equal ( "HEAD", "https://acc.r2.cloudflarestorage.com/uploads/incoming/" ++ idText ++ ".jpg" ) ( m.method, m.url )
                                , \m -> Expect.equal Nothing (header "host" m.headers)
                                , \m ->
                                    header "authorization" m.headers
                                        |> Maybe.map (String.contains "SignedHeaders=host;x-amz-content-sha256;x-amz-date,")
                                        |> Expect.equal (Just True)
                                ]
                                made
                        )
            , test "reads the size, the type and the ETag" <|
                \() ->
                    withRequest (R2.head settings time)
                        (\made ->
                            made.onResponse (response 200 [ ( "content-length", "12" ), ( "content-type", "image/jpeg" ), ( "etag", "\"abc\"" ) ] "")
                                |> Expect.equal (Ok (Just { bytes = 12, contentType = "image/jpeg", etag = "\"abc\"" }))
                        )
            , test "refuses an answer without an ETag" <|
                \() ->
                    withRequest (R2.head settings time)
                        (\made ->
                            made.onResponse (response 200 [ ( "content-length", "12" ), ( "content-type", "image/jpeg" ) ] "")
                                |> Result.toMaybe
                                |> Expect.equal Nothing
                        )
            , test "answers Nothing for a missing file" <|
                \() ->
                    withRequest (R2.head settings time)
                        (\made ->
                            made.onResponse (response 404 [] "")
                                |> Expect.equal (Ok Nothing)
                        )
            , test "refuses any other status" <|
                \() ->
                    withRequest (R2.head settings time)
                        (\made ->
                            made.onResponse (response 403 [] "<Error/>")
                                |> Expect.equal (Err "R2 answered 403: <Error/>")
                        )
            ]
        , describe "copyToPublic"
            [ test "sends an empty signed PUT that copies the staged file, pinned to its ETag" <|
                \() ->
                    withRequest (\found -> R2.copyToPublic settings time found "\"abc\"")
                        (\made ->
                            Expect.all
                                [ \m -> Expect.equal ( "PUT", "https://acc.r2.cloudflarestorage.com/pictures/" ++ idText ++ ".jpg" ) ( m.method, m.url )
                                , \m -> Expect.equal Effect.EmptyBody m.body
                                , \m -> Expect.equal (Just ("/uploads/incoming/" ++ idText ++ ".jpg")) (header "x-amz-copy-source" m.headers)
                                , \m -> Expect.equal (Just "\"abc\"") (header "x-amz-copy-source-if-match" m.headers)
                                , \m -> Expect.equal (Just "REPLACE") (header "x-amz-metadata-directive" m.headers)
                                , \m -> Expect.equal (Just "image/jpeg") (header "content-type" m.headers)
                                , \m -> Expect.equal (Just "public, max-age=3600") (header "cache-control" m.headers)
                                , \m ->
                                    header "authorization" m.headers
                                        |> Maybe.map (String.contains "SignedHeaders=cache-control;content-type;host;x-amz-content-sha256;x-amz-copy-source;x-amz-copy-source-if-match;x-amz-date;x-amz-metadata-directive,")
                                        |> Expect.equal (Just True)
                                ]
                                made
                        )
            , test "is Copied when the copy carries the same ETag" <|
                \() ->
                    withRequest (\found -> R2.copyToPublic settings time found "\"abc\"")
                        (\made ->
                            made.onResponse (response 200 [] "<CopyObjectResult><ETag>&quot;abc&quot;</ETag></CopyObjectResult>")
                                |> Expect.equal (Ok R2.Copied)
                        )
            , test "is Changed when the copy carries another ETag" <|
                \() ->
                    withRequest (\found -> R2.copyToPublic settings time found "\"abc\"")
                        (\made ->
                            made.onResponse (response 200 [] "<CopyObjectResult><ETag>\"zzz\"</ETag></CopyObjectResult>")
                                |> Expect.equal (Ok R2.Changed)
                        )
            , test "is Changed when the precondition failed" <|
                \() ->
                    withRequest (\found -> R2.copyToPublic settings time found "\"abc\"")
                        (\made ->
                            made.onResponse (response 412 [] "<Error><Code>PreconditionFailed</Code></Error>")
                                |> Expect.equal (Ok R2.Changed)
                        )
            , test "refuses an error inside a 200" <|
                \() ->
                    withRequest (\found -> R2.copyToPublic settings time found "\"abc\"")
                        (\made ->
                            made.onResponse (response 200 [] "<Error><Code>InternalError</Code></Error>")
                                |> Result.toMaybe
                                |> Expect.equal Nothing
                        )
            , test "refuses a 200 without an ETag" <|
                \() ->
                    withRequest (\found -> R2.copyToPublic settings time found "\"abc\"")
                        (\made ->
                            made.onResponse (response 200 [] "")
                                |> Result.toMaybe
                                |> Expect.equal Nothing
                        )
            , test "refuses any other status" <|
                \() ->
                    withRequest (\found -> R2.copyToPublic settings time found "\"abc\"")
                        (\made ->
                            made.onResponse (response 500 [] "<Error/>")
                                |> Expect.equal (Err "R2 answered 500: <Error/>")
                        )
            ]
        , describe "delete"
            [ test "sends a signed DELETE for the public key" <|
                \() ->
                    withRequest (R2.delete settings time)
                        (\made ->
                            ( made.method, made.url )
                                |> Expect.equal ( "DELETE", "https://acc.r2.cloudflarestorage.com/pictures/" ++ idText ++ ".jpg" )
                        )
            , test "answers Ok on 204 and on 404, and Err on 403" <|
                \() ->
                    withRequest (R2.delete settings time)
                        (\made ->
                            ( made.onResponse (response 204 [] "")
                            , made.onResponse (response 404 [] "")
                            , Result.toMaybe (made.onResponse (response 403 [] ""))
                            )
                                |> Expect.equal ( Ok (), Ok (), Nothing )
                        )
            ]
        , describe "deleteStaged"
            [ test "sends a signed DELETE for the staged key" <|
                \() ->
                    withRequest (R2.deleteStaged settings time)
                        (\made ->
                            ( made.method, made.url )
                                |> Expect.equal ( "DELETE", "https://acc.r2.cloudflarestorage.com/uploads/incoming/" ++ idText ++ ".jpg" )
                        )
            ]
        ]
