module SyncTest exposing (suite)

import Api exposing (SyncProvider(..))
import ApiClient
import Data.Username as Username
import Dict
import Effect exposing (Effect(..))
import Expect
import Http
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
        , describe "Google Drive"
            [ test "looks for the pixo folder first, with the token" <|
                \() ->
                    driveToken
                        |> Tuple.second
                        |> requestOf
                        |> Expect.equal
                            (Just
                                { method = "GET"
                                , url = "https://www.googleapis.com/drive/v3/files?q=name%20%3D%20'pixo'%20and%20mimeType%20%3D%20'application%2Fvnd.google-apps.folder'%20and%20trashed%20%3D%20false&fields=files(id)&spaces=drive"
                                , headers = [ ( "authorization", "Bearer t" ) ]
                                , body = ""
                                }
                            )
            , test "makes the folder when there is none, then uploads into My Drive" <|
                \() ->
                    let
                        ( model, job ) =
                            driveJob

                        ( looked, _ ) =
                            driveToken

                        ( _, creating ) =
                            Sync.update drivePictures (FolderFound job (good """{"files":[]}""")) looked

                        ( _, uploading ) =
                            Sync.update drivePictures (FolderMade job (good """{"id":"F"}""")) looked
                    in
                    Expect.equal
                        ( requestOf creating
                        , blobOf uploading
                        )
                        ( Just
                            { method = "POST"
                            , url = "https://www.googleapis.com/drive/v3/files"
                            , headers = [ ( "authorization", "Bearer t" ) ]
                            , body = """{"name":"pixo","mimeType":"application/vnd.google-apps.folder"}"""
                            }
                        , Just driveBlob
                        )
            , test "uploads at once when the folder exists" <|
                \() ->
                    let
                        ( _, job ) =
                            driveJob

                        ( looked, _ ) =
                            driveToken
                    in
                    Sync.update drivePictures (FolderFound job (good """{"files":[{"id":"F"}]}""")) looked
                        |> Tuple.second
                        |> Expect.all
                            [ blobOf >> Expect.equal (Just driveBlob)
                            , requestOf >> Expect.equal Nothing
                            ]
            , test "renames the upload into the folder, then finishes with its id" <|
                \() ->
                    let
                        ( model, job ) =
                            driveUploaded

                        ( _, renaming ) =
                            Sync.update drivePictures (Uploaded job { status = 200, body = """{"id":"X","parents":["R"]}""" }) model
                    in
                    Expect.equal
                        ( requestOf renaming
                        , Sync.finished model (Named job "X" (good "{}"))
                        )
                        ( Just
                            { method = "PATCH"
                            , url = "https://www.googleapis.com/drive/v3/files/X?addParents=F&removeParents=R&fields=id"
                            , headers = [ ( "authorization", "Bearer t" ) ]
                            , body = """{"name":"pixo-7.png"}"""
                            }
                        , Just ( 2, GoogleDrive, "X" )
                        )
            , test "skips the lookup for the next picture" <|
                \() ->
                    let
                        ( model, job ) =
                            driveUploaded

                        ( done, _ ) =
                            Sync.update drivePictures (Named job "X" (good "{}")) model

                        marked =
                            Tuple.first (Library.markSynced 2 GoogleDrive "X" drivePictures)

                        ( queued, _ ) =
                            Sync.next [ GoogleDrive ] marked done
                    in
                    case queued.job of
                        Just following ->
                            Sync.update marked (Timed following (Time.millisToPosix 0)) queued
                                |> Tuple.second
                                |> Expect.all
                                    [ blobOf >> Expect.equal (Just { driveBlob | label = "google-drive:1", url = "blob:1" })
                                    , requestOf >> Expect.equal Nothing
                                    ]

                        Nothing ->
                            Expect.fail "Expected another picture."
            , test "retries a refused rename once, then counts a failure" <|
                \() ->
                    let
                        ( model, job ) =
                            driveUploaded

                        ( retrying, effect ) =
                            Sync.update drivePictures (Named job "X" (refusedWith 401)) model

                        again =
                            { job | retried = True }

                        ( failed, _ ) =
                            Sync.update drivePictures (Named again "X" (refusedWith 401)) retrying
                    in
                    Expect.equal
                        ( ( retrying.job, Dict.isEmpty retrying.tokens, describeEffect effect )
                        , ( failed.job, Dict.toList failed.failures )
                        )
                        ( ( Just again, True, "time" )
                        , ( Nothing, [ ( "google-drive", 1 ) ] )
                        )
            ]
        ]


driveJob : ( Sync.Model, Sync.Job )
driveJob =
    let
        ( model, _ ) =
            Sync.next [ GoogleDrive ] drivePictures Sync.init
    in
    ( model, Maybe.withDefault { provider = GoogleDrive, pictureId = 0, retried = False } model.job )


driveToken : ( Sync.Model, Effect Msg )
driveToken =
    let
        ( model, job ) =
            driveJob
    in
    Sync.update drivePictures (GotToken job (Time.millisToPosix 0) (Ok { accessToken = "t", expiresIn = 14400 })) model


driveUploaded : ( Sync.Model, Sync.Job )
driveUploaded =
    let
        ( _, job ) =
            driveJob

        ( looked, _ ) =
            driveToken

        ( found, _ ) =
            Sync.update drivePictures (FolderFound job (good """{"files":[{"id":"F"}]}""")) looked
    in
    ( found, job )


drivePictures : Library.Model
drivePictures =
    library [ ( 2, Nothing ), ( 1, Nothing ) ]


driveBlob : BlobSummary
driveBlob =
    { label = "google-drive:2"
    , url = "blob:2"
    , method = "POST"
    , endpoint = "https://www.googleapis.com/upload/drive/v3/files?uploadType=media&fields=id,parents"
    , headers = [ ( "authorization", "Bearer t" ), ( "content-type", "image/png" ) ]
    , release = False
    }


type alias BlobSummary =
    { label : String
    , url : String
    , method : String
    , endpoint : String
    , headers : List ( String, String )
    , release : Bool
    }


blobOf : Effect Msg -> Maybe BlobSummary
blobOf effect =
    case effect of
        SendToJs (Interop.SendBlob blob) ->
            Just blob

        _ ->
            Nothing


requestOf : Effect Msg -> Maybe { method : String, url : String, headers : List ( String, String ), body : String }
requestOf effect =
    case effect of
        Request request ->
            Just
                { method = request.method
                , url = request.url
                , headers = request.headers
                , body = Maybe.withDefault "" (Maybe.map (Encode.encode 0) request.body)
                }

        _ ->
            Nothing


good : String -> Http.Response String
good body =
    Http.GoodStatus_ { url = "", statusCode = 200, statusText = "OK", headers = Dict.empty } body


refusedWith : Int -> Http.Response String
refusedWith status =
    Http.BadStatus_ { url = "", statusCode = status, statusText = "", headers = Dict.empty } ""


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
