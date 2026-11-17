module ApiTest exposing (suite)

import Api
import Data.CredentialId as CredentialId
import Data.PictureId as PictureId
import Data.Tag as Tag
import Data.Username as Username
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
                   , test "finds no passkey to delete at an empty id" <|
                        \() ->
                            Api.parse "DELETE" [ "api", "auth", "passkeys", "" ]
                                |> Expect.equal (Err Api.NotFound)
                   , test "finds no picture to delete at a bad id" <|
                        \() ->
                            Api.parse "DELETE" [ "api", "pictures", "nope" ]
                                |> Expect.equal (Err Api.NotFound)
                   , test "round-trips a tag with a space" <|
                        \() ->
                            case Tag.fromString "sea life" of
                                Just tag ->
                                    Api.parse "GET" (Api.path (Api.GetFeed (Just tag) Nothing))
                                        |> Expect.equal (Ok (Api.GetFeed (Just tag) Nothing))

                                Nothing ->
                                    Expect.fail "sea life is a tag"
                   , test "finds no feed at a tag that is not written canonically" <|
                        \() ->
                            Api.parse "GET" [ "api", "feed", "tag", "Fox" ]
                                |> Expect.equal (Err Api.NotFound)
                   , test "finds no profile at a too short name" <|
                        \() ->
                            Api.parse "GET" [ "api", "profiles", "a" ]
                                |> Expect.equal (Err Api.NotFound)
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
            [ test "encodes what it decodes" <|
                \() ->
                    Decode.decodeString (Api.responseDecoder Api.session) """{"email":"ada@example.com","username":"ada","billing":{"plan":"pro","renewsAt":"2026-12-17T10:00:00Z","endsAt":null,"pastDue":false,"canManage":true}}"""
                        |> Result.map (Api.encodeResponse Api.session >> Encode.encode 0)
                        |> Expect.equal (Ok """{"email":"ada@example.com","username":"ada","billing":{"plan":"pro","renewsAt":"2026-12-17T10:00:00Z","endsAt":null,"pastDue":false,"canManage":true}}""")
            , test "refuses an unknown plan" <|
                \() ->
                    Decode.decodeString (Api.responseDecoder Api.session) """{"email":"ada@example.com","username":"ada","billing":{"plan":"gold","renewsAt":null,"endsAt":null,"pastDue":false,"canManage":false}}"""
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]
        , describe "checkout"
            [ test "encodes what it decodes" <|
                \() ->
                    Decode.decodeString (Api.responseDecoder Api.checkout) """{"transactionId":"txn_01","clientToken":"test_token","environment":"sandbox"}"""
                        |> Result.map (Api.encodeResponse Api.checkout >> Encode.encode 0)
                        |> Expect.equal (Ok """{"transactionId":"txn_01","clientToken":"test_token","environment":"sandbox"}""")
            ]
        , describe "portal"
            [ test "encodes what it decodes" <|
                \() ->
                    Decode.decodeString (Api.responseDecoder Api.portal) """{"url":"https://example.com/x"}"""
                        |> Result.map (Api.encodeResponse Api.portal >> Encode.encode 0)
                        |> Expect.equal (Ok """{"url":"https://example.com/x"}""")
            ]
        , describe "publish"
            [ test "sends the metadata and the tags as JSON" <|
                \() ->
                    Api.encodeBody Api.publish
                        { prompt = "a red fox"
                        , width = 1024
                        , height = 1024
                        , steps = 4
                        , seed = 7
                        , model = "prism-ml/bonsai-image-ternary-4B-mlx-2bit"
                        , bytes = 312345
                        , tags = List.filterMap Tag.fromString [ "fox", "snow" ]
                        }
                        |> Maybe.map (Encode.encode 0)
                        |> Expect.equal (Just """{"prompt":"a red fox","width":1024,"height":1024,"steps":4,"seed":7,"model":"prism-ml/bonsai-image-ternary-4B-mlx-2bit","bytes":312345,"tags":["fox","snow"]}""")
            , test "decodes the body it encodes" <|
                \() ->
                    Decode.decodeString (Api.bodyDecoder Api.publish) """{"prompt":"a red fox","width":1024,"height":1024,"steps":4,"seed":7,"model":"m","bytes":12,"tags":["fox"]}"""
                        |> Result.map (\p -> ( p.prompt, p.bytes, List.map Tag.toString p.tags ))
                        |> Expect.equal (Ok ( "a red fox", 12, [ "fox" ] ))
            , test "refuses a body with one invalid tag" <|
                \() ->
                    Decode.decodeString (Api.bodyDecoder Api.publish) """{"prompt":"a red fox","width":1024,"height":1024,"steps":4,"seed":7,"model":"m","bytes":12,"tags":["fox","x"]}"""
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            , test "answers with the link and its headers" <|
                \() ->
                    let
                        json =
                            """{"id":"6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11","url":"https://acc.r2.cloudflarestorage.com/pixo-uploads/incoming/6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11.jpg?X-Amz-Algorithm=x","headers":[["content-type","image/jpeg"]]}"""
                    in
                    Decode.decodeString (Api.responseDecoder Api.publish) json
                        |> Result.map (Api.encodeResponse Api.publish >> Encode.encode 0)
                        |> Expect.equal (Ok json)
            ]
        , describe "confirm"
            [ test "answers with the public address" <|
                \() ->
                    let
                        json =
                            """{"id":"6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11","url":"https://img.pixo.pics/6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11.jpg","publishedAt":"2026-11-17T10:00:00Z"}"""
                    in
                    case PictureId.fromString exampleId of
                        Just id ->
                            Decode.decodeString (Api.responseDecoder (Api.confirm id)) json
                                |> Result.map (Api.encodeResponse (Api.confirm id) >> Encode.encode 0)
                                |> Expect.equal (Ok json)

                        Nothing ->
                            Expect.fail "The example id should parse."
            ]
        , describe "verification"
            [ test "encodes what it decodes" <|
                \() ->
                    Decode.decodeString (Api.responseDecoder Api.verification) """{"email":"ada@example.com","username":"ada","newAccount":true}"""
                        |> Result.map (Api.encodeResponse Api.verification >> Encode.encode 0)
                        |> Expect.equal (Ok """{"email":"ada@example.com","username":"ada","newAccount":true}""")
            ]
        , describe "verify"
            [ test "encodes what it decodes" <|
                \() ->
                    Decode.decodeString (Api.responseDecoder Api.verify) """{"email":"ada@example.com","username":"ada"}"""
                        |> Result.map (Api.encodeResponse Api.verify >> Encode.encode 0)
                        |> Expect.equal (Ok """{"email":"ada@example.com","username":"ada"}""")
            ]
        , describe "passkeys"
            [ test "decodes what it encodes" <|
                \() ->
                    let
                        json =
                            """[{"id":"oKGio6SlpqeoqaqrrK2urw","name":"Apple Passwords","createdAt":"2026-11-10T09:30:00Z"},{"id":"oKGio6SlpqeoqaqrrK2urw","name":null,"createdAt":"2026-11-10T09:30:00Z"}]"""
                    in
                    Decode.decodeString (Api.responseDecoder Api.passkeys) json
                        |> Result.map (Api.encodeResponse Api.passkeys >> Encode.encode 0)
                        |> Expect.equal (Ok json)
            ]
        , describe "passkeySignInOptions"
            [ test "decodes what it encodes" <|
                \() ->
                    let
                        json =
                            """{"challenge":"Y2hhbGxlbmdl","rpId":"pixo.pics"}"""
                    in
                    Decode.decodeString (Api.responseDecoder Api.passkeySignInOptions) json
                        |> Result.map (Api.encodeResponse Api.passkeySignInOptions >> Encode.encode 0)
                        |> Expect.equal (Ok json)
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
                    let
                        json =
                            """{"challenge":"Y2hhbGxlbmdl","rp":{"id":"pixo.pics","name":"pixo"},"user":{"id":"7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11","name":"ada@example.com","displayName":"ada"},"algorithms":[-8,-7,-257],"excludeCredentials":["oKGio6SlpqeoqaqrrK2urw"]}"""
                    in
                    Decode.decodeString (Api.responseDecoder Api.passkeyOptions) json
                        |> Result.map (Api.encodeResponse Api.passkeyOptions >> Encode.encode 0)
                        |> Expect.equal (Ok json)
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
                    let
                        json =
                            """{"id":"oKGio6SlpqeoqaqrrK2urw","name":"Apple Passwords","createdAt":"2026-11-10T09:30:00Z"}"""
                    in
                    Decode.decodeString (Api.responseDecoder Api.addPasskey) json
                        |> Result.map (Api.encodeResponse Api.addPasskey >> Encode.encode 0)
                        |> Expect.equal (Ok json)
            ]
        , describe "feed"
            [ test "decodes a page and keeps the tags that parse" <|
                \() ->
                    Decode.decodeString (Api.responseDecoder (Api.feed Nothing Nothing))
                        """{"pictures":[{"id":"6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11","url":"https://img.pixo.pics/6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11.jpg","username":"ada","prompt":"a red fox","width":1024,"height":1024,"steps":4,"seed":7,"model":"m","tags":["fox","x","snow"],"publishedAt":"2026-11-17T10:00:00Z"}],"next":"6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11"}"""
                        |> Result.map (\page -> ( List.map (.tags >> List.map Tag.toString) page.pictures, Maybe.map PictureId.toString page.next ))
                        |> Expect.equal (Ok ( [ [ "fox", "snow" ] ], Just exampleId ))
            , test "writes a page the way it reads it" <|
                \() ->
                    let
                        json =
                            """{"pictures":[{"id":"6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11","url":"https://img.pixo.pics/6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11.jpg","username":"ada","prompt":"a red fox","width":1024,"height":1024,"steps":4,"seed":7,"model":"m","tags":["fox","snow"],"publishedAt":"2026-11-17T10:00:00Z"}],"next":null}"""
                    in
                    Decode.decodeString (Api.responseDecoder (Api.feed Nothing Nothing)) json
                        |> Result.map (Api.encodeResponse (Api.feed Nothing Nothing) >> Encode.encode 0)
                        |> Expect.equal (Ok json)
            , test "writes the top tags the way it reads them" <|
                \() ->
                    let
                        json =
                            """[{"tag":"fox","pictures":12},{"tag":"sunset","pictures":9}]"""
                    in
                    Decode.decodeString (Api.responseDecoder Api.feedTags) json
                        |> Result.map (Api.encodeResponse Api.feedTags >> Encode.encode 0)
                        |> Expect.equal (Ok json)
            , test "writes a profile page the way it reads it" <|
                \() ->
                    let
                        json =
                            """{"username":"ada","pictures":[],"next":null}"""
                    in
                    case Username.claim "ada" of
                        Ok name ->
                            Decode.decodeString (Api.responseDecoder (Api.profile name Nothing)) json
                                |> Result.map (Api.encodeResponse (Api.profile name Nothing) >> Encode.encode 0)
                                |> Expect.equal (Ok json)

                        Err reason ->
                            Expect.fail reason
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
            Just Api.DeleteAccount

        Api.DeleteAccount ->
            Just Api.GetPasskeys

        Api.GetPasskeys ->
            Just Api.PostPasskey

        Api.PostPasskey ->
            Just Api.PostPasskeyOptions

        Api.PostPasskeyOptions ->
            Maybe.map Api.DeletePasskey (CredentialId.fromString "oKGio6SlpqeoqaqrrK2urw")

        Api.DeletePasskey id ->
            if CredentialId.toString id == "oKGio6SlpqeoqaqrrK2urw" then
                Maybe.map Api.DeletePasskey (CredentialId.fromString "options")

            else
                Just Api.PostPasskeySignIn

        Api.PostPasskeySignIn ->
            Just Api.PostPasskeySignInOptions

        Api.PostPasskeySignInOptions ->
            Just Api.PostBillingWebhook

        Api.PostBillingWebhook ->
            Just Api.PostCheckout

        Api.PostCheckout ->
            Just Api.PostPortal

        Api.PostPortal ->
            Just Api.PostPicture

        Api.PostPicture ->
            Maybe.map Api.PostPicturePublished (PictureId.fromString exampleId)

        Api.PostPicturePublished id ->
            Just (Api.DeletePicture id)

        Api.DeletePicture _ ->
            Just (Api.GetFeed Nothing Nothing)

        Api.GetFeed Nothing Nothing ->
            Just (Api.GetFeed Nothing (PictureId.fromString exampleId))

        Api.GetFeed Nothing (Just _) ->
            Just (Api.GetFeed (Tag.fromString "fox") Nothing)

        Api.GetFeed (Just _) Nothing ->
            Just (Api.GetFeed (Tag.fromString "fox") (PictureId.fromString exampleId))

        Api.GetFeed (Just _) (Just _) ->
            Just Api.GetFeedTags

        Api.GetFeedTags ->
            Maybe.map (\name -> Api.GetProfile name Nothing) (Result.toMaybe (Username.claim "ada"))

        Api.GetProfile name Nothing ->
            Just (Api.GetProfile name (PictureId.fromString exampleId))

        Api.GetProfile _ (Just _) ->
            Nothing


exampleId : String
exampleId =
    "6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11"


assertion : Api.Assertion
assertion =
    { id = "oKGio6SlpqeoqaqrrK2urw"
    , clientDataJSON = "eyJ0eXBlIjo"
    , authenticatorData = "hc4asGtQ"
    , signature = "MEQCIQCA"
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
