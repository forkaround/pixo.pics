module ApiTest exposing (suite)

import Api
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Api"
        [ describe "parse"
            (List.map
                (\route ->
                    test ("finds " ++ Api.methodToString (Api.method route) ++ " " ++ Api.toString route) <|
                        \() ->
                            Api.parse (Api.methodToString (Api.method route)) (Api.path route)
                                |> Expect.equal (Ok route)
                )
                routes
                ++ [ test "names the methods a path does allow" <|
                        \() ->
                            Api.parse "POST" [ "api", "health" ]
                                |> Expect.equal (Err (Api.MethodNotAllowed [ Api.Get ]))
                   , test "names both methods of a path that has two" <|
                        \() ->
                            Api.parse "DELETE" [ "api", "auth", "verify" ]
                                |> Expect.equal (Err (Api.MethodNotAllowed [ Api.Get, Api.Post ]))
                   , test "tells a path from the longer one that starts with it" <|
                        \() ->
                            Api.parse "GET" [ "api", "auth", "passkeys", "options" ]
                                |> Expect.equal (Err (Api.MethodNotAllowed [ Api.Post, Api.Delete ]))
                   , test "finds nothing at an unknown path" <|
                        \() ->
                            Api.parse "GET" [ "api", "nowhere" ]
                                |> Expect.equal (Err Api.NotFound)
                   ]
            )
        , describe "signUp"
            [ test "sends the email and the username as JSON" <|
                \() ->
                    Api.encodeBody Api.signUp { email = "ada@example.com", username = "ada" }
                        |> Maybe.map (Encode.encode 0)
                        |> Expect.equal (Just """{"email":"ada@example.com","username":"ada"}""")
            , test "decodes the body it encodes" <|
                \() ->
                    Decode.decodeString (Api.bodyDecoder Api.signUp) """{"email":"ada@example.com","username":"ada"}"""
                        |> Expect.equal (Ok { email = "ada@example.com", username = "ada" })
            , test "answers with an empty object" <|
                \() ->
                    Api.encodeResponse Api.signUp ()
                        |> Encode.encode 0
                        |> Expect.equal "{}"
            ]
        , describe "signIn"
            [ test "sends the email as JSON" <|
                \() ->
                    Api.encodeBody Api.signIn { email = "ada@example.com" }
                        |> Maybe.map (Encode.encode 0)
                        |> Expect.equal (Just """{"email":"ada@example.com"}""")
            , test "decodes the body it encodes" <|
                \() ->
                    Decode.decodeString (Api.bodyDecoder Api.signIn) """{"email":"ada@example.com"}"""
                        |> Expect.equal (Ok { email = "ada@example.com" })
            ]
        , describe "session"
            [ test "decodes what it encodes" <|
                \() ->
                    Api.encodeResponse Api.session { email = "ada@example.com", username = "ada" }
                        |> Decode.decodeValue (Api.responseDecoder Api.session)
                        |> Expect.equal (Ok { email = "ada@example.com", username = "ada" })
            ]
        , describe "verification"
            [ test "decodes what it encodes" <|
                \() ->
                    Api.encodeResponse Api.verification { email = "ada@example.com", username = "ada", newAccount = True }
                        |> Decode.decodeValue (Api.responseDecoder Api.verification)
                        |> Expect.equal (Ok { email = "ada@example.com", username = "ada", newAccount = True })
            ]
        , describe "verify"
            [ test "decodes what it encodes" <|
                \() ->
                    Api.encodeResponse Api.verify { email = "ada@example.com", username = "ada" }
                        |> Decode.decodeValue (Api.responseDecoder Api.verify)
                        |> Expect.equal (Ok { email = "ada@example.com", username = "ada" })
            ]
        , describe "passkeys"
            [ test "decodes what it encodes" <|
                \() ->
                    Api.encodeResponse Api.passkeys [ passkey, { passkey | name = Nothing } ]
                        |> Decode.decodeValue (Api.responseDecoder Api.passkeys)
                        |> Expect.equal (Ok [ passkey, { passkey | name = Nothing } ])
            ]
        , describe "passkeySignInOptions"
            [ test "decodes what it encodes" <|
                \() ->
                    Api.encodeResponse Api.passkeySignInOptions { challenge = "Y2hhbGxlbmdl", rpId = "pixo.pics" }
                        |> Decode.decodeValue (Api.responseDecoder Api.passkeySignInOptions)
                        |> Expect.equal (Ok { challenge = "Y2hhbGxlbmdl", rpId = "pixo.pics" })
            ]
        , describe "passkeySignIn"
            [ test "sends what the passkey signed as JSON" <|
                \() ->
                    Api.encodeBody Api.passkeySignIn assertion
                        |> Maybe.map (Encode.encode 0)
                        |> Expect.equal
                            (Just """{"id":"oKGio6SlpqeoqaqrrK2urw","clientDataJSON":"eyJ0eXBlIjo","authenticatorData":"hc4asGtQ","signature":"MEQCIQCA"}""")
            , test "decodes the body it encodes" <|
                \() ->
                    Decode.decodeString (Api.bodyDecoder Api.passkeySignIn)
                        """{"id":"oKGio6SlpqeoqaqrrK2urw","clientDataJSON":"eyJ0eXBlIjo","authenticatorData":"hc4asGtQ","signature":"MEQCIQCA"}"""
                        |> Expect.equal (Ok assertion)
            ]
        , describe "passkeyOptions"
            [ test "decodes what it encodes" <|
                \() ->
                    Api.encodeResponse Api.passkeyOptions passkeyOptions
                        |> Decode.decodeValue (Api.responseDecoder Api.passkeyOptions)
                        |> Expect.equal (Ok passkeyOptions)
            ]
        , describe "addPasskey"
            [ test "sends what the browser made as JSON" <|
                \() ->
                    Api.encodeBody Api.addPasskey
                        { clientDataJSON = "eyJ0eXBlIjo", attestationObject = "o2NmbXRkbm9uZQ", transports = [ "internal" ] }
                        |> Maybe.map (Encode.encode 0)
                        |> Expect.equal
                            (Just """{"clientDataJSON":"eyJ0eXBlIjo","attestationObject":"o2NmbXRkbm9uZQ","transports":["internal"]}""")
            , test "decodes the body it encodes" <|
                \() ->
                    Decode.decodeString (Api.bodyDecoder Api.addPasskey)
                        """{"clientDataJSON":"eyJ0eXBlIjo","attestationObject":"o2NmbXRkbm9uZQ","transports":["internal"]}"""
                        |> Expect.equal
                            (Ok { clientDataJSON = "eyJ0eXBlIjo", attestationObject = "o2NmbXRkbm9uZQ", transports = [ "internal" ] })
            , test "answers with the passkey" <|
                \() ->
                    Api.encodeResponse Api.addPasskey passkey
                        |> Encode.encode 0
                        |> Expect.equal """{"id":"oKGio6SlpqeoqaqrrK2urw","name":"Apple Passwords","createdAt":"2026-11-10T09:30:00Z"}"""
            ]
        , describe "health"
            [ test "decodes what it encodes" <|
                \() ->
                    Api.encodeResponse Api.health report
                        |> Decode.decodeValue (Api.responseDecoder Api.health)
                        |> Expect.equal (Ok report)
            ]
        ]


routes : List Api.Route
routes =
    collect (Just Api.GetHealth) []


collect : Maybe Api.Route -> List Api.Route -> List Api.Route
collect route found =
    case route of
        Just current ->
            collect (next current) (current :: found)

        Nothing ->
            List.reverse found


next : Api.Route -> Maybe Api.Route
next route =
    case route of
        Api.GetHealth ->
            Just Api.PostSignUp

        Api.PostSignUp ->
            Just Api.PostSignIn

        Api.PostSignIn ->
            Just Api.GetVerification

        Api.GetVerification ->
            Just Api.PostVerification

        Api.PostVerification ->
            Just Api.GetSession

        Api.GetSession ->
            Just Api.DeleteSession

        Api.DeleteSession ->
            Just Api.GetPasskeys

        Api.GetPasskeys ->
            Just Api.PostPasskey

        Api.PostPasskey ->
            Just Api.PostPasskeyOptions

        Api.PostPasskeyOptions ->
            Just (Api.DeletePasskey "oKGio6SlpqeoqaqrrK2urw")

        Api.DeletePasskey "oKGio6SlpqeoqaqrrK2urw" ->
            Just (Api.DeletePasskey "options")

        Api.DeletePasskey _ ->
            Just Api.PostPasskeySignIn

        Api.PostPasskeySignIn ->
            Just Api.PostPasskeySignInOptions

        Api.PostPasskeySignInOptions ->
            Nothing


assertion : Api.Assertion
assertion =
    { id = "oKGio6SlpqeoqaqrrK2urw"
    , clientDataJSON = "eyJ0eXBlIjo"
    , authenticatorData = "hc4asGtQ"
    , signature = "MEQCIQCA"
    }


passkey : Api.Passkey
passkey =
    { id = "oKGio6SlpqeoqaqrrK2urw", name = Just "Apple Passwords", createdAt = "2026-11-10T09:30:00Z" }


passkeyOptions : Api.PasskeyOptions
passkeyOptions =
    { challenge = "Y2hhbGxlbmdl"
    , rp = { id = "pixo.pics", name = "pixo" }
    , user = { id = "7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11", name = "ada@example.com", displayName = "ada" }
    , algorithms = [ -8, -7, -257 ]
    , excludeCredentials = [ "oKGio6SlpqeoqaqrrK2urw" ]
    }


report : Api.Health
report =
    Api.healthOf
        { releaseId = Just "064160704a6b52143928d03db8e50fcd5dfe0205"
        , releaseUrl = Just "https://github.com/forkaround/pixo.pics/commit/064160704a6b52143928d03db8e50fcd5dfe0205"
        , region = Just "arn1"
        }
        { status = Api.Warn
        , latencyMs = Just 1200
        , output = Just "Slow"
        }
