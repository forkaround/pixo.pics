module SyncTest exposing (suite)

import Api exposing (SyncProvider(..))
import ApiClient
import Data.Username as Username
import Dict
import Effect exposing (Effect(..))
import Expect
import Idb
import InteropDefinitions as Interop
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Library
import Sync exposing (Msg(..))
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "Sync"
        [ describe "next"
            [ test "picks the newest picture without a Dropbox entry" <|
                \() ->
                    Sync.next [ Dropbox ] (library [ ( 3, Just "/pixo-7.png" ), ( 2, Nothing ), ( 1, Nothing ) ]) Sync.init
                        |> Tuple.first
                        |> .job
                        |> Expect.equal (Just { provider = Dropbox, pictureId = 2, retried = False })
            , test "does nothing when every picture has one" <|
                \() ->
                    Sync.next [ Dropbox ] (library [ ( 2, Just "/a.png" ), ( 1, Just "/b.png" ) ]) Sync.init
                        |> Tuple.first
                        |> .job
                        |> Expect.equal Nothing
            , test "does nothing for a stopped provider" <|
                \() ->
                    let
                        ( model, job ) =
                            started

                        ( stopped, _ ) =
                            Sync.update pictures (GotToken job (Time.millisToPosix 0) (Err (ApiClient.Refused "Dropbox revoked access."))) model
                    in
                    Sync.next [ Dropbox ] pictures stopped
                        |> Tuple.first
                        |> .job
                        |> Expect.equal Nothing
            ]
        , describe "tokens"
            [ test "refreshes one with 30 seconds left" <|
                \() ->
                    let
                        ( model, job ) =
                            withToken
                    in
                    Sync.update pictures (Timed job (Time.millisToPosix (14400000 - 30000))) model
                        |> Tuple.second
                        |> describeEffect
                        |> Expect.equal "request POST /api/sync/dropbox/token"
            , test "reuses one with 10 minutes left" <|
                \() ->
                    let
                        ( model, job ) =
                            withToken
                    in
                    Sync.update pictures (Timed job (Time.millisToPosix (14400000 - 600000))) model
                        |> Tuple.second
                        |> describeEffect
                        |> Expect.equal "blob https://content.dropboxapi.com/2/files/upload"
            , test "stops the provider when the server refuses a token" <|
                \() ->
                    let
                        ( model, job ) =
                            started
                    in
                    Sync.update pictures (GotToken job (Time.millisToPosix 0) (Err (ApiClient.Refused "Dropbox access was revoked."))) model
                        |> Tuple.first
                        |> .stopped
                        |> Dict.toList
                        |> Expect.equal [ ( "dropbox", "Dropbox access was revoked." ) ]
            ]
        , describe "uploads"
            [ test "finish with the path Dropbox answers" <|
                \() ->
                    let
                        ( model, job ) =
                            withToken
                    in
                    Sync.finished model (Uploaded job { status = 200, body = """{"path_display":"/pixo-7.png"}""" })
                        |> Expect.equal (Just ( 2, Dropbox, "/pixo-7.png" ))
            , test "send the file name to Dropbox as JSON" <|
                \() ->
                    let
                        ( model, job ) =
                            started
                    in
                    Sync.update pictures (GotToken job (Time.millisToPosix 0) (Ok { accessToken = "t", expiresIn = 14400 })) model
                        |> Tuple.second
                        |> blobHeaders
                        |> Dict.fromList
                        |> Dict.get "dropbox-api-arg"
                        |> Maybe.andThen (Decode.decodeString (Decode.field "path" Decode.string) >> Result.toMaybe)
                        |> Expect.equal (Just "/pixo-7.png")
            , test "retry once after a 401, then count a failure" <|
                \() ->
                    let
                        ( model, job ) =
                            withToken

                        ( retrying, effect ) =
                            Sync.update pictures (Uploaded job { status = 401, body = "" }) model

                        again =
                            { job | retried = True }

                        ( failed, _ ) =
                            Sync.update pictures (Uploaded again { status = 401, body = "" }) retrying
                    in
                    Expect.equal
                        ( ( retrying.job, Dict.isEmpty retrying.tokens, describeEffect effect )
                        , ( failed.job, Dict.toList failed.failures )
                        )
                        ( ( Just again, True, "time" )
                        , ( Nothing, [ ( "dropbox", 1 ) ] )
                        )
            , test "stop the provider after three failures, and resume on request" <|
                \() ->
                    let
                        ( one, _ ) =
                            failOnce Sync.init

                        ( two, _ ) =
                            failOnce one

                        ( three, effect ) =
                            failOnce two

                        ( resumed, _ ) =
                            Sync.update pictures (Resumed Dropbox) three
                    in
                    Expect.equal
                        ( ( Dict.toList three.stopped, describeEffect effect )
                        , ( Dict.toList resumed.stopped, Dict.toList resumed.failures )
                        )
                        ( ( [ ( "dropbox", "Network offline" ) ], "notify Dropbox sync stopped" )
                        , ( [], [] )
                        )
            ]
        ]


failOnce : Sync.Model -> ( Sync.Model, Effect Msg )
failOnce model =
    let
        ( queued, _ ) =
            Sync.next [ Dropbox ] pictures model
    in
    case queued.job of
        Just job ->
            Sync.update pictures (UploadFailed job "Network offline") queued

        Nothing ->
            ( queued, NoEffect )


started : ( Sync.Model, Sync.Job )
started =
    let
        ( model, _ ) =
            Sync.next [ Dropbox ] pictures Sync.init
    in
    ( model, Maybe.withDefault { provider = Dropbox, pictureId = 0, retried = False } model.job )


withToken : ( Sync.Model, Sync.Job )
withToken =
    let
        ( model, job ) =
            started
    in
    ( Tuple.first (Sync.update pictures (GotToken job (Time.millisToPosix 0) (Ok { accessToken = "t", expiresIn = 14400 })) model), job )


pictures : Library.Model
pictures =
    library [ ( 3, Just "/pixo-7.png" ), ( 2, Nothing ), ( 1, Just "/pixo-7.png" ) ]


library : List ( Int, Maybe String ) -> Library.Model
library entries =
    let
        owned =
            case Username.parse "ada" of
                Ok name ->
                    Tuple.first (Library.signedIn name (Library.init False))

                Err _ ->
                    Library.init False
    in
    Library.update
        (Library.Happened (Library.Answered (Idb.Rows { label = "", rows = List.map row entries })))
        (Tuple.first (Library.enter owned))
        |> Tuple.first


row : ( Int, Maybe String ) -> Value
row ( id, reference ) =
    Encode.object
        ([ ( "id", Encode.int id )
         , ( "blob", Encode.string ("blob:" ++ String.fromInt id) )
         , ( "prompt", Encode.string "a red fox in the snow" )
         , ( "width", Encode.int 512 )
         , ( "height", Encode.int 512 )
         , ( "steps", Encode.int 4 )
         , ( "seed", Encode.int 7 )
         , ( "model", Encode.string "prism-ml/bonsai-image-ternary-4B-mlx-2bit" )
         , ( "owner", Encode.string "ada" )
         ]
            ++ (case reference of
                    Just found ->
                        [ ( "synced", Encode.object [ ( "dropbox", Encode.string found ) ] ) ]

                    Nothing ->
                        []
               )
        )


describeEffect : Effect Msg -> String
describeEffect effect =
    case effect of
        NoEffect ->
            "none"

        Request { method, url } ->
            "request " ++ method ++ " " ++ url

        SendToJs (Interop.SendBlob blob) ->
            "blob " ++ blob.endpoint

        ReadTime _ ->
            "time"

        Notify notice ->
            "notify " ++ notice.title

        _ ->
            "other"


blobHeaders : Effect Msg -> List ( String, String )
blobHeaders effect =
    case effect of
        SendToJs (Interop.SendBlob blob) ->
            blob.headers

        _ ->
            []
