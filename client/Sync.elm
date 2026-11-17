module Sync exposing (Job, Model, Msg(..), Status(..), byLabel, finished, init, next, status, update)

import Api exposing (SyncProvider(..))
import ApiClient
import Dict exposing (Dict)
import Effect exposing (Effect(..))
import Http
import InteropDefinitions as Interop
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Library
import Notice
import Time
import Url


type alias Model =
    { tokens : Dict String { accessToken : String, expiresAt : Int }
    , folder : Maybe String
    , job : Maybe Job
    , failures : Dict String Int
    , stopped : Dict String String
    }


type alias Job =
    { provider : SyncProvider
    , pictureId : Int
    , retried : Bool
    }


type Status
    = Off
    | Copying { done : Int, total : Int }
    | Done { total : Int }
    | Stopped String


type Msg
    = Timed Job Time.Posix
    | GotToken Job Time.Posix (Result ApiClient.Error Api.SyncToken)
    | FolderFound Job (Http.Response String)
    | FolderMade Job (Http.Response String)
    | Uploaded Job { status : Int, body : String }
    | UploadFailed Job String
    | Named Job String (Http.Response String)
    | Resumed SyncProvider


init : Model
init =
    { tokens = Dict.empty
    , folder = Nothing
    , job = Nothing
    , failures = Dict.empty
    , stopped = Dict.empty
    }


toast : String
toast =
    "cloud-sync"


key : SyncProvider -> String
key =
    Api.providerToString


label : Job -> String
label job =
    key job.provider ++ ":" ++ String.fromInt job.pictureId


byLabel : String -> Model -> Maybe Job
byLabel name model =
    model.job
        |> Maybe.andThen
            (\job ->
                if label job == name then
                    Just job

                else
                    Nothing
            )


finished : Model -> Msg -> Maybe ( Int, SyncProvider, String )
finished model msg =
    case msg of
        Uploaded job result ->
            if model.job == Just job && result.status == 200 && job.provider == Dropbox then
                Decode.decodeString (Decode.field "path_display" Decode.string) result.body
                    |> Result.toMaybe
                    |> Maybe.map (\reference -> ( job.pictureId, job.provider, reference ))

            else
                Nothing

        Named job reference (Http.GoodStatus_ _ _) ->
            if model.job == Just job then
                Just ( job.pictureId, job.provider, reference )

            else
                Nothing

        _ ->
            Nothing


next : List SyncProvider -> Library.Model -> Model -> ( Model, Effect Msg )
next connected library model =
    case model.job of
        Just _ ->
            ( model, NoEffect )

        Nothing ->
            let
                waiting =
                    List.concatMap
                        (\picture ->
                            List.filter
                                (\provider ->
                                    not (Dict.member (key provider) model.stopped)
                                        && not (Dict.member (key provider) picture.synced)
                                )
                                connected
                                |> List.map (\provider -> { provider = provider, pictureId = picture.id, retried = False })
                        )
                        (Library.loadedPictures library)
            in
            case waiting of
                job :: _ ->
                    ( { model | job = Just job }, ReadTime (Timed job) )

                [] ->
                    ( model, NoEffect )


status : List SyncProvider -> SyncProvider -> Library.Model -> Model -> Status
status connected provider library model =
    if not (List.member provider connected) then
        Off

    else
        case Dict.get (key provider) model.stopped of
            Just reason ->
                Stopped reason

            Nothing ->
                let
                    pictures =
                        Library.loadedPictures library

                    done =
                        List.length (List.filter (\picture -> Dict.member (key provider) picture.synced) pictures)

                    total =
                        List.length pictures
                in
                if done < total then
                    Copying { done = done, total = total }

                else
                    Done { total = total }


update : Library.Model -> Msg -> Model -> ( Model, Effect Msg )
update library msg model =
    case msg of
        Resumed provider ->
            ( { model
                | stopped = Dict.remove (key provider) model.stopped
                , failures = Dict.remove (key provider) model.failures
              }
            , Dismiss toast
            )

        Timed job now ->
            current job model (\() -> proceed library job now model)

        GotToken job now (Ok token) ->
            current job
                model
                (\() ->
                    proceed library
                        job
                        now
                        { model
                            | tokens =
                                Dict.insert (key job.provider)
                                    { accessToken = token.accessToken
                                    , expiresAt = Time.posixToMillis now + token.expiresIn * 1000
                                    }
                                    model.tokens
                        }
                )

        GotToken job _ (Err (ApiClient.Refused message)) ->
            current job model (\() -> stop job.provider message model)

        GotToken job _ (Err error) ->
            current job model (\() -> fail job (ApiClient.errorToString error) model)

        FolderFound job response ->
            current job
                model
                (\() ->
                    case response of
                        Http.GoodStatus_ _ body ->
                            case Decode.decodeString foundDecoder body of
                                Ok (Just folder) ->
                                    upload library job { model | folder = Just folder }

                                Ok Nothing ->
                                    authorised job model (\token -> ( model, Request (makeFolder token (FolderMade job)) ))

                                Err _ ->
                                    fail job "Unexpected response" model

                        _ ->
                            refused job response model
                )

        FolderMade job response ->
            current job
                model
                (\() ->
                    case response of
                        Http.GoodStatus_ _ body ->
                            case Decode.decodeString (Decode.field "id" Decode.string) body of
                                Ok folder ->
                                    upload library job { model | folder = Just folder }

                                Err _ ->
                                    fail job "Unexpected response" model

                        _ ->
                            refused job response model
                )

        Uploaded job result ->
            current job model (\() -> uploaded library job result model)

        UploadFailed job message ->
            current job model (\() -> fail job message model)

        Named job _ response ->
            current job
                model
                (\() ->
                    case response of
                        Http.GoodStatus_ _ _ ->
                            succeed job model

                        _ ->
                            refused job response model
                )


current : Job -> Model -> (() -> ( Model, Effect Msg )) -> ( Model, Effect Msg )
current job model run =
    if model.job == Just job then
        run ()

    else
        ( model, NoEffect )


succeed : Job -> Model -> ( Model, Effect Msg )
succeed job model =
    ( { model | job = Nothing, failures = Dict.remove (key job.provider) model.failures }, NoEffect )


proceed : Library.Model -> Job -> Time.Posix -> Model -> ( Model, Effect Msg )
proceed library job now model =
    case Library.find job.pictureId library of
        Nothing ->
            ( { model | job = Nothing }, NoEffect )

        Just _ ->
            case Dict.get (key job.provider) model.tokens of
                Just token ->
                    if token.expiresAt - Time.posixToMillis now > 60000 then
                        case ( job.provider, model.folder ) of
                            ( GoogleDrive, Nothing ) ->
                                ( model, Request (findFolder token.accessToken (FolderFound job)) )

                            _ ->
                                upload library job model

                    else
                        askForToken job now model

                Nothing ->
                    askForToken job now model


askForToken : Job -> Time.Posix -> Model -> ( Model, Effect Msg )
askForToken job now model =
    ( model, Request (ApiClient.request (Api.syncToken job.provider) () (GotToken job now)) )


authorised : Job -> Model -> (String -> ( Model, Effect Msg )) -> ( Model, Effect Msg )
authorised job model run =
    case Dict.get (key job.provider) model.tokens of
        Just token ->
            run token.accessToken

        Nothing ->
            ( model, ReadTime (Timed job) )


upload : Library.Model -> Job -> Model -> ( Model, Effect Msg )
upload library job model =
    case Library.find job.pictureId library of
        Nothing ->
            ( { model | job = Nothing }, NoEffect )

        Just picture ->
            authorised job
                model
                (\token ->
                    let
                        name =
                            Library.fileName picture.job

                        ( endpoint, headers ) =
                            case job.provider of
                                Dropbox ->
                                    ( "https://content.dropboxapi.com/2/files/upload"
                                    , [ ( "authorization", "Bearer " ++ token )
                                      , ( "content-type", "application/octet-stream" )
                                      , ( "dropbox-api-arg", dropboxArgument name )
                                      ]
                                    )

                                GoogleDrive ->
                                    ( "https://www.googleapis.com/upload/drive/v3/files?uploadType=media&fields=id,parents"
                                    , [ ( "authorization", "Bearer " ++ token )
                                      , ( "content-type", "image/png" )
                                      ]
                                    )
                    in
                    ( model
                    , SendToJs
                        (Interop.SendBlob
                            { label = label job
                            , url = picture.url
                            , method = "POST"
                            , endpoint = endpoint
                            , headers = headers
                            , release = False
                            }
                        )
                    )
                )


dropboxArgument : String -> String
dropboxArgument name =
    Encode.encode 0
        (Encode.object
            [ ( "path", Encode.string ("/" ++ name) )
            , ( "mode", Encode.string "add" )
            , ( "autorename", Encode.bool True )
            , ( "mute", Encode.bool True )
            ]
        )


uploaded : Library.Model -> Job -> { status : Int, body : String } -> Model -> ( Model, Effect Msg )
uploaded library job result model =
    if result.status == 200 then
        case job.provider of
            Dropbox ->
                case Decode.decodeString (Decode.field "path_display" Decode.string) result.body of
                    Ok _ ->
                        succeed job model

                    Err _ ->
                        fail job "Unexpected response" model

            GoogleDrive ->
                case ( Decode.decodeString driveFileDecoder result.body, model.folder ) of
                    ( Ok file, Just folder ) ->
                        authorised job model (\token -> ( model, Request (nameFile token folder file (fileNameOf library job) (Named job file.id)) ))

                    _ ->
                        fail job "Unexpected response" model

    else if result.status == 401 then
        unauthorised job model

    else
        fail job ("Upload refused with HTTP " ++ String.fromInt result.status) model


refused : Job -> Http.Response String -> Model -> ( Model, Effect Msg )
refused job response model =
    case response of
        Http.BadStatus_ metadata _ ->
            if metadata.statusCode == 401 then
                unauthorised job model

            else
                fail job ("Request refused with HTTP " ++ String.fromInt metadata.statusCode) model

        Http.GoodStatus_ _ _ ->
            fail job "Unexpected response" model

        _ ->
            fail job "Network offline" model


unauthorised : Job -> Model -> ( Model, Effect Msg )
unauthorised job model =
    let
        forgotten =
            { model | tokens = Dict.remove (key job.provider) model.tokens }
    in
    if job.retried then
        fail job "The token was refused" forgotten

    else
        let
            again =
                { job | retried = True }
        in
        ( { forgotten | job = Just again }, ReadTime (Timed again) )


fail : Job -> String -> Model -> ( Model, Effect Msg )
fail job reason model =
    let
        count =
            Maybe.withDefault 0 (Dict.get (key job.provider) model.failures) + 1
    in
    if count >= 3 then
        stop job.provider reason model

    else
        ( { model | job = Nothing, failures = Dict.insert (key job.provider) count model.failures }, NoEffect )


stop : SyncProvider -> String -> Model -> ( Model, Effect Msg )
stop provider reason model =
    ( { model
        | job = Nothing
        , failures = Dict.remove (key provider) model.failures
        , stopped = Dict.insert (key provider) reason model.stopped
      }
    , Notify
        { id = toast
        , tone = Notice.Problem
        , title = Api.providerName provider ++ " sync stopped"
        , description = Just reason
        , retry = Notice.Retry (Resumed provider)
        }
    )


foundDecoder : Decoder (Maybe String)
foundDecoder =
    Decode.field "files" (Decode.list (Decode.field "id" Decode.string))
        |> Decode.map List.head


driveFileDecoder : Decoder { id : String, parents : List String }
driveFileDecoder =
    Decode.map2 (\id parents -> { id = id, parents = parents })
        (Decode.field "id" Decode.string)
        (Decode.oneOf [ Decode.field "parents" (Decode.list Decode.string), Decode.succeed [] ])


bearer : String -> List ( String, String )
bearer token =
    [ ( "authorization", "Bearer " ++ token ) ]


findFolder : String -> (Http.Response String -> msg) -> ApiClient.Request msg
findFolder token onResponse =
    { method = "GET"
    , headers = bearer token
    , url =
        "https://www.googleapis.com/drive/v3/files?q="
            ++ Url.percentEncode "name = 'pixo' and mimeType = 'application/vnd.google-apps.folder' and trashed = false"
            ++ "&fields=files(id)&spaces=drive"
    , body = Nothing
    , onResponse = onResponse
    }


makeFolder : String -> (Http.Response String -> msg) -> ApiClient.Request msg
makeFolder token onResponse =
    { method = "POST"
    , headers = bearer token
    , url = "https://www.googleapis.com/drive/v3/files"
    , body =
        Just
            (Encode.object
                [ ( "name", Encode.string "pixo" )
                , ( "mimeType", Encode.string "application/vnd.google-apps.folder" )
                ]
            )
    , onResponse = onResponse
    }


nameFile : String -> String -> { id : String, parents : List String } -> String -> (Http.Response String -> msg) -> ApiClient.Request msg
nameFile token folder file name onResponse =
    { method = "PATCH"
    , headers = bearer token
    , url =
        "https://www.googleapis.com/drive/v3/files/"
            ++ Url.percentEncode file.id
            ++ "?addParents="
            ++ Url.percentEncode folder
            ++ (if List.isEmpty file.parents then
                    ""

                else
                    "&removeParents=" ++ String.join "," (List.map Url.percentEncode file.parents)
               )
            ++ "&fields=id"
    , body = Just (Encode.object [ ( "name", Encode.string name ) ])
    , onResponse = onResponse
    }


fileNameOf : Library.Model -> Job -> String
fileNameOf library job =
    Library.find job.pictureId library
        |> Maybe.map (.job >> Library.fileName)
        |> Maybe.withDefault "pixo.png"
