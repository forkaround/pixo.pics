module Page.Profile exposing (Model, Msg, Outcome(..), init, lightbox, outcome, toasts, update, view)

import Api
import ApiClient
import Data.PictureId exposing (PictureId)
import Data.Username as Username exposing (Username)
import Effect exposing (Effect(..))
import FeedView
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (attribute, class, disabled)
import Html.Events exposing (onClick)
import Notice
import Overlay exposing (glass)
import Plain
import Session exposing (Session(..))
import UI.Button as Button
import UI.Empty as Empty
import UI.Skeleton as Skeleton



-- MODEL


type Page
    = Loading
    | Missing
    | Shown (List Api.FeedPicture)


type alias Model =
    { name : Username
    , page : Page
    , next : Maybe PictureId
    , more : Bool
    , viewing : Maybe PictureId
    , detailed : Bool
    , removing : Maybe PictureId
    }


init : Username -> ( Model, Effect Msg )
init name =
    ( { name = name
      , page = Loading
      , next = Nothing
      , more = False
      , viewing = Nothing
      , detailed = False
      , removing = Nothing
      }
    , fetch name
    )


fetch : Username -> Effect Msg
fetch name =
    Request (ApiClient.request (Api.profile name Nothing) () GotProfile)



-- UPDATE


type Msg
    = GotProfile (Result ApiClient.Error Api.ProfilePage)
    | ProfileRetried
    | MoreClicked
    | GotMore (Result ApiClient.Error Api.ProfilePage)
    | Opened PictureId
    | Closed
    | Stepped Int
    | DetailsToggled
    | UnpublishClicked PictureId
    | GotUnpublished PictureId (Result ApiClient.Error ())


type Outcome
    = Unchanged
    | Unpublished PictureId


outcome : Msg -> Outcome
outcome msg =
    case msg of
        GotUnpublished id (Ok ()) ->
            Unpublished id

        GotUnpublished id (Err (ApiClient.Refused _)) ->
            Unpublished id

        _ ->
            Unchanged


update : Session -> Msg -> Model -> ( Model, Effect Msg )
update session msg model =
    case msg of
        GotProfile (Ok page) ->
            ( { model | page = Shown page.pictures, next = page.next }, Dismiss toast )

        GotProfile (Err (ApiClient.Refused _)) ->
            ( { model | page = Missing }, Dismiss toast )

        GotProfile (Err error) ->
            ( model
            , Effect.failure { id = toast, title = "Can't load this profile", retry = Just ProfileRetried } error
            )

        ProfileRetried ->
            ( { model | page = Loading, next = Nothing, more = False }, fetch model.name )

        MoreClicked ->
            case ( model.more, model.next ) of
                ( False, Just before ) ->
                    ( { model | more = True }
                    , Request (ApiClient.request (Api.profile model.name (Just before)) () GotMore)
                    )

                _ ->
                    ( model, NoEffect )

        GotMore (Ok page) ->
            ( { model
                | page =
                    case model.page of
                        Shown shown ->
                            Shown (shown ++ page.pictures)

                        _ ->
                            Shown page.pictures
                , next = page.next
                , more = False
              }
            , Dismiss toast
            )

        GotMore (Err error) ->
            ( { model | more = False }
            , Effect.failure { id = toast, title = "Can't load this profile", retry = Just MoreClicked } error
            )

        Opened id ->
            ( { model | viewing = Just id }, NoEffect )

        Closed ->
            ( { model | viewing = Nothing }, NoEffect )

        Stepped offset ->
            case ( model.page, model.viewing ) of
                ( Shown pictures, Just viewed ) ->
                    case FeedView.neighbour offset viewed pictures of
                        Just id ->
                            ( { model | viewing = Just id }, NoEffect )

                        Nothing ->
                            ( model, NoEffect )

                _ ->
                    ( model, NoEffect )

        DetailsToggled ->
            ( { model | detailed = not model.detailed }, NoEffect )

        UnpublishClicked id ->
            if mine session model && model.removing == Nothing then
                ( { model | removing = Just id }
                , Request (ApiClient.request (Api.unpublish id) () (GotUnpublished id))
                )

            else
                ( model, NoEffect )

        GotUnpublished id result ->
            case result of
                Err (ApiClient.Refused _) ->
                    ( removed id model, unpublished )

                Ok () ->
                    ( removed id model, unpublished )

                Err error ->
                    ( { model | removing = Nothing }
                    , Effect.failure { id = toast, title = "Couldn't unpublish the picture", retry = Nothing } error
                    )


removed : PictureId -> Model -> Model
removed id model =
    { model
        | page =
            case model.page of
                Shown pictures ->
                    Shown (List.filter (\picture -> picture.id /= id) pictures)

                other ->
                    other
        , removing = Nothing
        , viewing =
            if model.viewing == Just id then
                Nothing

            else
                model.viewing
    }


unpublished : Effect msg
unpublished =
    Notify { id = toast, tone = Notice.Success, title = "Unpublished", description = Nothing, retry = Notice.NoRetry }


mine : Session -> Model -> Bool
mine session model =
    case session of
        SignedIn account ->
            account.username == model.name

        _ ->
            False


toast : String
toast =
    "profile"


toasts : List String
toasts =
    [ toast ]



-- VIEW


view : Session -> Model -> Html Msg
view _ model =
    case model.page of
        Missing ->
            div [ class "p-4 pt-8 md:p-8" ] [ Plain.lead "Not found" "There is nothing at this address." ]

        Loading ->
            div [ class "flex flex-col gap-6 p-4 pt-8" ]
                [ Skeleton.view [ class "h-8 w-40", attribute "aria-hidden" "true" ]
                , FeedView.skeleton
                ]

        Shown [] ->
            div [ class "flex flex-1 flex-col gap-6 p-4 pt-8" ]
                [ heading model
                , Empty.new
                    |> Empty.title [ text "No pictures yet" ]
                    |> Empty.toHtml []
                ]

        Shown pictures ->
            div [ class "flex flex-col gap-6 p-4 pt-8" ]
                [ heading model
                , FeedView.grid (List.map (FeedView.tile { open = Opened, author = False }) pictures)
                , case model.next of
                    Just _ ->
                        div [ class "flex justify-center" ]
                            [ Button.new
                                |> Button.text
                                    (if model.more then
                                        "Loading more…"

                                     else
                                        "Load more"
                                    )
                                |> Button.outline
                                |> Button.toHtml [ onClick MoreClicked, disabled model.more ]
                            ]

                    Nothing ->
                        text ""
                ]


heading : Model -> Html msg
heading model =
    h1 [ class "text-2xl font-semibold tracking-tight" ] [ text (Username.toString model.name) ]


lightbox : (Msg -> msg) -> Session -> Html msg -> Model -> Html msg
lightbox to session toastLayer model =
    FeedView.lightbox
        { id = "profile-picture"
        , onClose = to Closed
        , onToggle = to DetailsToggled
        , onStep = to << Stepped
        , toasts = toastLayer
        , detailed = model.detailed
        }
        (case ( model.page, model.viewing ) of
            ( Shown pictures, Just viewed ) ->
                pictures
                    |> List.filter (\picture -> picture.id == viewed)
                    |> List.head
                    |> Maybe.map (\picture -> ( picture, List.map (Html.map to) (actions session model picture) ))

            _ ->
                Nothing
        )


actions : Session -> Model -> Api.FeedPicture -> List (Html Msg)
actions session model picture =
    if mine session model then
        let
            busy =
                model.removing == Just picture.id
        in
        [ Button.new
            |> Button.text
                (if busy then
                    "Unpublishing…"

                 else
                    "Unpublish"
                )
            |> Button.small
            |> Button.toHtml [ onClick (UnpublishClicked picture.id), disabled busy, glass ]
        ]

    else
        []
