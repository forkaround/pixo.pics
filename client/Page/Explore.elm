module Page.Explore exposing (Model, Msg, init, lightbox, toasts, update, view)

import Api
import ApiClient
import Data.PictureId exposing (PictureId)
import Data.Tag as Tag exposing (Tag)
import Effect exposing (Effect(..))
import ExploreView
import FeedView
import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, class, disabled)
import Html.Events exposing (onClick)
import Overlay exposing (glass)
import UI.Button as Button
import UI.Empty as Empty
import UI.Skeleton as Skeleton



-- MODEL


type Feed
    = Loading
    | Loaded (List Api.FeedPicture)


type Tags
    = TagsLoading
    | TagsLoaded (List Api.TagCount)


type alias Model =
    { feed : Feed
    , tag : Maybe Tag
    , tags : Tags
    , next : Maybe PictureId
    , more : Bool
    , viewing : Maybe PictureId
    , detailed : Bool
    }


init : ( Model, Effect Msg )
init =
    ( { feed = Loading
      , tag = Nothing
      , tags = TagsLoading
      , next = Nothing
      , more = False
      , viewing = Nothing
      , detailed = False
      }
    , Batch [ fetch Nothing, fetchTags ]
    )


fetch : Maybe Tag -> Effect Msg
fetch tag =
    Request (ApiClient.request (Api.feed tag Nothing) () (GotFeed tag))


fetchTags : Effect Msg
fetchTags =
    Request (ApiClient.request Api.feedTags () GotTags)



-- UPDATE


type Msg
    = GotFeed (Maybe Tag) (Result ApiClient.Error Api.FeedPage)
    | FeedRetried
    | MoreClicked
    | GotMore (Maybe Tag) (Result ApiClient.Error Api.FeedPage)
    | GotTags (Result ApiClient.Error (List Api.TagCount))
    | TagsRetried
    | TagChosen (Maybe Tag)
    | Opened PictureId
    | Closed
    | Stepped Int
    | DetailsToggled


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotFeed tag (Ok page) ->
            if tag /= model.tag then
                ( model, NoEffect )

            else
                ( { model | feed = Loaded page.pictures, next = page.next }, Dismiss feedToast )

        GotFeed tag (Err error) ->
            if tag /= model.tag then
                ( model, NoEffect )

            else
                ( model
                , Effect.failure { id = feedToast, title = "Can't load the feed", retry = Just FeedRetried } error
                )

        FeedRetried ->
            ( { model | feed = Loading, next = Nothing, more = False }, fetch model.tag )

        MoreClicked ->
            case ( model.more, model.next ) of
                ( False, Just before ) ->
                    ( { model | more = True }
                    , Request (ApiClient.request (Api.feed model.tag (Just before)) () (GotMore model.tag))
                    )

                _ ->
                    ( model, NoEffect )

        GotMore tag result ->
            if tag /= model.tag then
                ( model, NoEffect )

            else
                case result of
                    Ok page ->
                        ( { model
                            | feed =
                                case model.feed of
                                    Loaded shown ->
                                        Loaded (shown ++ page.pictures)

                                    Loading ->
                                        Loaded page.pictures
                            , next = page.next
                            , more = False
                          }
                        , Dismiss feedToast
                        )

                    Err error ->
                        ( { model | more = False }
                        , Effect.failure { id = feedToast, title = "Can't load the feed", retry = Just MoreClicked } error
                        )

        GotTags (Ok found) ->
            ( { model | tags = TagsLoaded found }, Dismiss tagsToast )

        GotTags (Err error) ->
            ( model
            , Effect.failure { id = tagsToast, title = "Can't load the tags", retry = Just TagsRetried } error
            )

        TagsRetried ->
            ( { model | tags = TagsLoading }, fetchTags )

        TagChosen tag ->
            if tag == model.tag then
                ( { model | viewing = Nothing }, NoEffect )

            else
                ( { model | tag = tag, feed = Loading, next = Nothing, more = False, viewing = Nothing }
                , Batch [ Dismiss feedToast, fetch tag ]
                )

        Opened id ->
            ( { model | viewing = Just id }, NoEffect )

        Closed ->
            ( { model | viewing = Nothing }, NoEffect )

        Stepped offset ->
            case ( model.feed, model.viewing ) of
                ( Loaded pictures, Just viewed ) ->
                    case FeedView.neighbour offset viewed pictures of
                        Just id ->
                            ( { model | viewing = Just id }, NoEffect )

                        Nothing ->
                            ( model, NoEffect )

                _ ->
                    ( model, NoEffect )

        DetailsToggled ->
            ( { model | detailed = not model.detailed }, NoEffect )


feedToast : String
feedToast =
    "feed"


tagsToast : String
tagsToast =
    "feed-tags"


toasts : List String
toasts =
    [ feedToast, tagsToast ]



-- VIEW


view : ExploreView.Invitation -> Model -> Html Msg
view invitation model =
    ExploreView.view invitation
        (div [ class "flex flex-col gap-6" ]
            [ viewTags model
            , viewFeed model
            ]
        )


viewTags : Model -> Html Msg
viewTags model =
    case model.tags of
        TagsLoading ->
            div [ class "flex gap-2", attribute "aria-hidden" "true" ]
                (List.repeat 6 (Skeleton.view [ class "h-8 w-20 shrink-0 rounded-full" ]))

        TagsLoaded counts ->
            let
                top =
                    List.map .tag counts

                extra =
                    case model.tag of
                        Just tag ->
                            if List.member tag top then
                                []

                            else
                                [ tag ]

                        Nothing ->
                            []
            in
            if List.isEmpty top && List.isEmpty extra then
                text ""

            else
                row model (top ++ extra)


row : Model -> List Tag -> Html Msg
row model tags =
    div [ class "-mx-4 flex gap-2 overflow-x-auto px-4 pb-1 [scrollbar-width:none]" ]
        (chip (model.tag == Nothing) (TagChosen Nothing) "All"
            :: List.map (\tag -> chip (model.tag == Just tag) (TagChosen (Just tag)) (Tag.toString tag)) tags
        )


chip : Bool -> Msg -> String -> Html Msg
chip chosen clicked label =
    Button.new
        |> Button.text label
        |> Button.small
        |> Button.rounded
        |> (if chosen then
                identity

            else
                Button.outline
           )
        |> Button.toHtml
            [ onClick clicked
            , attribute "aria-pressed"
                (if chosen then
                    "true"

                 else
                    "false"
                )
            , class "shrink-0"
            ]


viewFeed : Model -> Html Msg
viewFeed model =
    case ( model.feed, model.tag ) of
        ( Loading, Nothing ) ->
            ExploreView.decorative

        ( Loaded [], Nothing ) ->
            ExploreView.decorative

        ( Loading, Just _ ) ->
            FeedView.skeleton

        ( Loaded [], Just _ ) ->
            Empty.new
                |> Empty.title [ text "No pictures with this tag yet" ]
                |> Empty.toHtml []

        ( Loaded pictures, _ ) ->
            div [ class "flex flex-col gap-6" ]
                [ FeedView.grid (List.map (FeedView.tile { open = Opened, author = True }) pictures)
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


lightbox : (Msg -> msg) -> Html msg -> Model -> Html msg
lightbox to toastLayer model =
    FeedView.lightbox
        { id = "feed-picture"
        , onClose = to Closed
        , onToggle = to DetailsToggled
        , onStep = to << Stepped
        , toasts = toastLayer
        , detailed = model.detailed
        }
        (case ( model.feed, model.viewing ) of
            ( Loaded pictures, Just viewed ) ->
                pictures
                    |> List.filter (\picture -> picture.id == viewed)
                    |> List.head
                    |> Maybe.map (\picture -> ( picture, List.map (Html.map to << tagButton) picture.tags ))

            _ ->
                Nothing
        )


tagButton : Tag -> Html Msg
tagButton tag =
    Button.new
        |> Button.text (Tag.toString tag)
        |> Button.small
        |> Button.toHtml [ onClick (TagChosen (Just tag)), glass ]
