module Publish exposing (Model, Msg(..), Outcome(..), Step(..), init, label, outcome, step, toasts, update)

import Api
import ApiClient
import Data.Dimension as Dimension
import Data.PictureId exposing (PictureId)
import Data.Prompt as Prompt
import Data.Seed as Seed
import Data.Steps as Steps
import Dict exposing (Dict)
import Effect exposing (Effect(..))
import Idb
import InteropDefinitions as Interop
import Library
import Notice


type alias Model =
    { confirming : Maybe Int
    , busy : Dict Int Step
    }


type Step
    = Encoding
    | Asking
    | Uploading PictureId
    | Confirming PictureId
    | Unpublishing


type Msg
    = PublishRequested Int
    | PublishCancelled
    | PublishConfirmed Int
    | Encoded Int { url : String, bytes : Int }
    | EncodeFailed Int String
    | GotUpload Int String (Result ApiClient.Error Api.Upload)
    | Sent Int { status : Int, body : String }
    | SendFailed Int String
    | GotPublished Int (Result ApiClient.Error Api.Published)
    | UnpublishClicked Int PictureId
    | GotUnpublished Int (Result ApiClient.Error ())
    | UploadCancelled (Result ApiClient.Error ())


type Outcome
    = Unchanged
    | Marked Int (Maybe PictureId)


init : Model
init =
    { confirming = Nothing, busy = Dict.empty }


quality : Float
quality =
    0.9


toast : String
toast =
    "publish"


toasts : List String
toasts =
    [ toast ]


label : Int -> String
label id =
    "publish:" ++ String.fromInt id


step : Int -> Model -> Maybe Step
step id model =
    Dict.get id model.busy


outcome : Msg -> Outcome
outcome msg =
    case msg of
        GotPublished id (Ok published) ->
            Marked id (Just published.id)

        GotUnpublished id (Ok ()) ->
            Marked id Nothing

        GotUnpublished id (Err (ApiClient.Refused _)) ->
            Marked id Nothing

        _ ->
            Unchanged


update : Library.Model -> Msg -> Model -> ( Model, Effect Msg )
update library msg model =
    case msg of
        PublishRequested id ->
            ( { model | confirming = Just id }, NoEffect )

        PublishCancelled ->
            ( { model | confirming = Nothing }, NoEffect )

        PublishConfirmed id ->
            case ( Library.find id library, step id model ) of
                ( Just picture, Nothing ) ->
                    ( { model | confirming = Nothing, busy = Dict.insert id Encoding model.busy }
                    , SendToJs (Interop.EncodePicture { label = label id, url = picture.url, quality = quality })
                    )

                _ ->
                    ( { model | confirming = Nothing }, NoEffect )

        Encoded id jpeg ->
            case ( Library.find id library, step id model ) of
                ( Just picture, Just Encoding ) ->
                    ( { model | busy = Dict.insert id Asking model.busy }
                    , Request
                        (ApiClient.request Api.publish
                            { prompt = Prompt.toString picture.job.prompt
                            , width = Dimension.toInt picture.job.width
                            , height = Dimension.toInt picture.job.height
                            , steps = Steps.toInt picture.job.steps
                            , seed = Seed.toInt picture.job.seed
                            , model = picture.model
                            , bytes = jpeg.bytes
                            , tags = Maybe.withDefault [] picture.tags
                            }
                            (GotUpload id jpeg.url)
                        )
                    )

                _ ->
                    ( model, release jpeg.url )

        EncodeFailed id message ->
            ( drop id model
            , Notify (problem message)
            )

        GotUpload id jpegUrl (Ok upload) ->
            if step id model == Just Asking then
                ( { model | busy = Dict.insert id (Uploading upload.id) model.busy }
                , SendToJs
                    (Interop.SendBlob
                        { label = label id
                        , url = jpegUrl
                        , method = "PUT"
                        , endpoint = upload.url
                        , headers = upload.headers
                        , release = True
                        }
                    )
                )

            else
                ( model, release jpegUrl )

        GotUpload id jpegUrl (Err error) ->
            ( drop id model
            , Batch
                [ release jpegUrl
                , Effect.failure { id = toast, title = title, retry = Just (PublishConfirmed id) } error
                ]
            )

        Sent id { status } ->
            if status >= 200 && status < 300 then
                case step id model of
                    Just (Uploading remote) ->
                        ( { model | busy = Dict.insert id (Confirming remote) model.busy }
                        , Request (ApiClient.request (Api.confirm remote) () (GotPublished id))
                        )

                    _ ->
                        ( model, NoEffect )

            else
                uploadFailed id model

        SendFailed id _ ->
            uploadFailed id model

        GotPublished id (Ok _) ->
            ( drop id model
            , Notify { id = toast, tone = Notice.Success, title = "Published", description = Nothing, retry = Notice.NoRetry }
            )

        GotPublished id (Err error) ->
            ( drop id model
            , Effect.failure { id = toast, title = title, retry = Just (PublishConfirmed id) } error
            )

        UnpublishClicked id remote ->
            if step id model == Nothing then
                ( { model | busy = Dict.insert id Unpublishing model.busy }
                , Request (ApiClient.request (Api.unpublish remote) () (GotUnpublished id))
                )

            else
                ( model, NoEffect )

        GotUnpublished id (Ok ()) ->
            ( drop id model, unpublished )

        GotUnpublished id (Err (ApiClient.Refused _)) ->
            ( drop id model, unpublished )

        GotUnpublished id (Err error) ->
            ( drop id model
            , Effect.failure { id = toast, title = "Couldn't unpublish the picture", retry = Nothing } error
            )

        UploadCancelled _ ->
            ( model, NoEffect )


title : String
title =
    "Couldn't publish the picture"


problem : String -> Notice.Notice msg
problem description =
    { id = toast, tone = Notice.Problem, title = title, description = Just description, retry = Notice.NoRetry }


unpublished : Effect msg
unpublished =
    Notify { id = toast, tone = Notice.Success, title = "Unpublished", description = Nothing, retry = Notice.NoRetry }


drop : Int -> Model -> Model
drop id model =
    { model | busy = Dict.remove id model.busy }


release : String -> Effect msg
release url =
    SendToJs (Interop.RunLibrary (Library.Database (Idb.Release [ url ])))


uploadFailed : Int -> Model -> ( Model, Effect Msg )
uploadFailed id model =
    case step id model of
        Just (Uploading remote) ->
            ( drop id model
            , Batch
                [ Request (ApiClient.request (Api.unpublish remote) () UploadCancelled)
                , Notify (problem "The upload didn't go through.")
                ]
            )

        _ ->
            ( model, NoEffect )
