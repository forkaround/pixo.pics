module OAuthTest exposing (suite)

import Api
import Effect
import Expect exposing (Expectation)
import Http
import Pkce
import Services.OAuth as OAuth
import Test exposing (Test, describe, test)
import Test.Http
import Url


suite : Test
suite =
    describe "OAuth"
        [ describe "Pkce.challenge"
            [ test "matches the example in RFC 7636" <|
                \() ->
                    Pkce.challenge "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
                        |> Expect.equal "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
            ]
        , describe "config"
            [ test "is nothing when nothing is set" <|
                \() ->
                    OAuth.config none
                        |> Expect.equal (Ok Nothing)
            , test "accepts Dropbox alone with a key" <|
                \() ->
                    OAuth.config { none | dropboxClientId = Just "dbx-id", dropboxClientSecret = Just "dbx-secret", syncTokenKey = Just key }
                        |> Expect.equal (Ok (Just { key = key, dropbox = Just { id = "dbx-id", secret = "dbx-secret" }, googleDrive = Nothing }))
            , test "accepts Google alone with a key" <|
                \() ->
                    OAuth.config { none | googleClientId = Just "g-id", googleClientSecret = Just "g-secret", syncTokenKey = Just key }
                        |> Expect.equal (Ok (Just { key = key, dropbox = Nothing, googleDrive = Just { id = "g-id", secret = "g-secret" } }))
            , test "refuses half a pair and names the variables" <|
                \() ->
                    OAuth.config { none | dropboxClientId = Just "dbx-id", syncTokenKey = Just key }
                        |> Result.mapError (String.contains "DROPBOX_CLIENT_SECRET")
                        |> Expect.equal (Err True)
            , test "refuses a pair without a key" <|
                \() ->
                    OAuth.config { none | googleClientId = Just "g-id", googleClientSecret = Just "g-secret" }
                        |> Result.mapError (String.contains "SYNC_TOKEN_KEY")
                        |> Expect.equal (Err True)
            , test "refuses a key shorter than 32 characters" <|
                \() ->
                    OAuth.config { none | googleClientId = Just "g-id", googleClientSecret = Just "g-secret", syncTokenKey = Just (String.left 31 key) }
                        |> Result.mapError (String.contains "32")
                        |> Expect.equal (Err True)
            , test "refuses a key with no client" <|
                \() ->
                    OAuth.config { none | syncTokenKey = Just key }
                        |> Result.mapError (String.contains "SYNC_TOKEN_KEY")
                        |> Expect.equal (Err True)
            ]
        , describe "authorizeUrl"
            [ test "asks Dropbox for an App folder code with PKCE and offline access" <|
                \() ->
                    OAuth.authorizeUrl Api.Dropbox dropbox wanted
                        |> parts
                        |> Expect.equal
                            ( "https://www.dropbox.com/oauth2/authorize"
                            , [ ( "client_id", "dbx-id" )
                              , ( "code_challenge", "chal" )
                              , ( "code_challenge_method", "S256" )
                              , ( "redirect_uri", "https://pixo.pics/settings/sync" )
                              , ( "response_type", "code" )
                              , ( "state", "st" )
                              , ( "token_access_type", "offline" )
                              ]
                            )
            , test "asks Google for the drive.file scope, offline, with consent" <|
                \() ->
                    OAuth.authorizeUrl Api.GoogleDrive google wanted
                        |> parts
                        |> Expect.equal
                            ( "https://accounts.google.com/o/oauth2/v2/auth"
                            , [ ( "access_type", "offline" )
                              , ( "client_id", "g-id" )
                              , ( "code_challenge", "chal" )
                              , ( "code_challenge_method", "S256" )
                              , ( "include_granted_scopes", "true" )
                              , ( "prompt", "consent" )
                              , ( "redirect_uri", "https://pixo.pics/settings/sync" )
                              , ( "response_type", "code" )
                              , ( "scope", "https://www.googleapis.com/auth/drive.file" )
                              , ( "state", "st" )
                              ]
                            )
            , test "never carries the client secret" <|
                \() ->
                    OAuth.authorizeUrl Api.GoogleDrive google wanted
                        |> String.contains "g-secret"
                        |> Expect.equal False
            ]
        , describe "exchange"
            [ test "posts the code and the verifier to Google as a form" <|
                \() ->
                    withHttp (OAuth.exchange Api.GoogleDrive google { code = "the-code", verifier = "the-verifier", redirectUri = "https://pixo.pics/settings/sync" })
                        (\made ->
                            { method = made.method, url = made.url, body = made.body, timeout = made.timeout }
                                |> Expect.equal
                                    { method = "POST"
                                    , url = "https://oauth2.googleapis.com/token"
                                    , body =
                                        Effect.FormBody
                                            [ ( "code", "the-code" )
                                            , ( "grant_type", "authorization_code" )
                                            , ( "redirect_uri", "https://pixo.pics/settings/sync" )
                                            , ( "code_verifier", "the-verifier" )
                                            , ( "client_id", "g-id" )
                                            , ( "client_secret", "g-secret" )
                                            ]
                                    , timeout = Just 10000
                                    }
                        )
            , test "posts to Dropbox's token address" <|
                \() ->
                    withHttp (OAuth.exchange Api.Dropbox dropbox { code = "c", verifier = "v", redirectUri = "r" })
                        (.url >> Expect.equal "https://api.dropboxapi.com/oauth2/token")
            , test "reads the tokens, with the refresh token" <|
                \() ->
                    answeredBy (OAuth.exchange Api.Dropbox dropbox { code = "c", verifier = "v", redirectUri = "r" })
                        (response 200 """{"access_token":"sl.a","expires_in":14400,"refresh_token":"r1","token_type":"bearer"}""")
                        |> Expect.equal (Just (Ok { accessToken = "sl.a", expiresIn = 14400, refreshToken = Just "r1" }))
            , test "never puts the secret or the code in an error" <|
                \() ->
                    answeredBy (OAuth.exchange Api.GoogleDrive google { code = "the-code", verifier = "the-verifier", redirectUri = "r" })
                        (response 400 """{"error":"invalid_request","error_description":"the-code g-secret"}""")
                        |> Expect.equal (Just (Err (OAuth.Failed "Google Drive answered 400 (invalid_request).")))
            ]
        , describe "refresh"
            [ test "posts the refresh token to Google as a form" <|
                \() ->
                    withHttp (OAuth.refresh Api.GoogleDrive google "refresh-1")
                        (\made ->
                            ( made.url, made.body )
                                |> Expect.equal
                                    ( "https://oauth2.googleapis.com/token"
                                    , Effect.FormBody
                                        [ ( "grant_type", "refresh_token" )
                                        , ( "refresh_token", "refresh-1" )
                                        , ( "client_id", "g-id" )
                                        , ( "client_secret", "g-secret" )
                                        ]
                                    )
                        )
            , test "reads a 200 as tokens with no new refresh token" <|
                \() ->
                    answeredBy (OAuth.refresh Api.GoogleDrive google "refresh-1")
                        (response 200 """{"access_token":"ya29.x","expires_in":3599}""")
                        |> Expect.equal (Just (Ok { accessToken = "ya29.x", expiresIn = 3599, refreshToken = Nothing }))
            , test "reads a 400 invalid_grant as revoked" <|
                \() ->
                    answeredBy (OAuth.refresh Api.GoogleDrive google "refresh-1")
                        (response 400 """{"error":"invalid_grant","error_description":"Token has been expired or revoked."}""")
                        |> Expect.equal (Just (Err OAuth.Revoked))
            , test "reads a 500 as a failure with its status" <|
                \() ->
                    answeredBy (OAuth.refresh Api.Dropbox dropbox "refresh-1")
                        (response 500 "oops")
                        |> Expect.equal (Just (Err (OAuth.Failed "Dropbox answered 500.")))
            , test "tells a timeout from an unreachable network" <|
                \() ->
                    ( answeredBy (OAuth.refresh Api.Dropbox dropbox "r") Http.Timeout_
                    , answeredBy (OAuth.refresh Api.Dropbox dropbox "r") Http.NetworkError_
                    )
                        |> Expect.equal
                            ( Just (Err (OAuth.Failed "Dropbox took too long to answer."))
                            , Just (Err (OAuth.Failed "Could not reach Dropbox."))
                            )
            ]
        , describe "revoke"
            [ test "posts the refresh token to Google as a form" <|
                \() ->
                    withHttp (OAuth.revoke Api.GoogleDrive google { refreshToken = "refresh-1", accessToken = Nothing })
                        (\made ->
                            ( made.url, made.body )
                                |> Expect.equal ( "https://oauth2.googleapis.com/revoke", Effect.FormBody [ ( "token", "refresh-1" ) ] )
                        )
            , test "posts an empty body to Dropbox with the access token" <|
                \() ->
                    withHttp (OAuth.revoke Api.Dropbox dropbox { refreshToken = "refresh-1", accessToken = Just "sl.a" })
                        (\made ->
                            ( made.url, made.headers, made.body )
                                |> Expect.equal
                                    ( "https://api.dropboxapi.com/2/auth/token/revoke"
                                    , [ ( "authorization", "Bearer sl.a" ) ]
                                    , Effect.EmptyBody
                                    )
                        )
            , test "asks Dropbox for nothing without an access token" <|
                \() ->
                    case OAuth.revoke Api.Dropbox dropbox { refreshToken = "refresh-1", accessToken = Nothing } of
                        Effect.Succeed (Ok ()) ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected no request."
            , test "reads any 2xx as done" <|
                \() ->
                    answeredBy (OAuth.revoke Api.GoogleDrive google { refreshToken = "r", accessToken = Nothing })
                        (response 200 "")
                        |> Expect.equal (Just (Ok ()))
            ]
        ]


none :
    { dropboxClientId : Maybe String
    , dropboxClientSecret : Maybe String
    , googleClientId : Maybe String
    , googleClientSecret : Maybe String
    , syncTokenKey : Maybe String
    }
none =
    { dropboxClientId = Nothing
    , dropboxClientSecret = Nothing
    , googleClientId = Nothing
    , googleClientSecret = Nothing
    , syncTokenKey = Nothing
    }


key : String
key =
    "0123456789abcdef0123456789abcdef0123"


dropbox : OAuth.Client
dropbox =
    { id = "dbx-id", secret = "dbx-secret" }


google : OAuth.Client
google =
    { id = "g-id", secret = "g-secret" }


wanted : { redirectUri : String, state : String, challenge : String }
wanted =
    { redirectUri = "https://pixo.pics/settings/sync", state = "st", challenge = "chal" }


parts : String -> ( String, List ( String, String ) )
parts address =
    case Url.fromString address of
        Just url ->
            ( "https://" ++ url.host ++ url.path
            , url.query
                |> Maybe.withDefault ""
                |> String.split "&"
                |> List.filterMap
                    (\pair ->
                        case String.split "=" pair of
                            [ name, value ] ->
                                Maybe.map2 Tuple.pair (Url.percentDecode name) (Url.percentDecode value)

                            _ ->
                                Nothing
                    )
                |> List.sort
            )

        Nothing ->
            ( "not a URL: " ++ address, [] )


withHttp : Effect.Effect a -> (Effect.HttpRequest a -> Expectation) -> Expectation
withHttp effect toExpectation =
    case effect of
        Effect.Http made ->
            toExpectation made

        _ ->
            Expect.fail "Expected an HTTP request."


answeredBy : Effect.Effect a -> Http.Response String -> Maybe a
answeredBy effect httpResponse =
    case effect of
        Effect.Http made ->
            Just (made.onResponse httpResponse)

        _ ->
            Nothing


response : Int -> String -> Http.Response String
response statusCode body =
    Test.Http.httpResponse { statusCode = statusCode, headers = [], body = body }
