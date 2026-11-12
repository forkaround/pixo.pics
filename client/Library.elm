module Library exposing
    ( Command(..)
    , Event(..)
    , Model
    , Msg(..)
    , Notice(..)
    , Picture
    , Pictures(..)
    , Save(..)
    , Stored
    , enter
    , fileName
    , find
    , init
    , modelName
    , saveOf
    , update
    , visible
    )

import Bm25
import Data.Dimension as Dimension
import Data.Prompt as Prompt
import Data.Seed as Seed
import Data.Steps as Steps
import Pipeline
import Share



-- MODEL


type alias Model =
    { pictures : Pictures
    , index : Bm25.Index
    , query : String
    , viewing : Maybe Int
    , zoomed : Bool
    , detailed : Bool
    , confirming : Maybe Int
    , saves : List ( String, Save )
    , notice : Maybe Notice
    , canShare : Bool
    }


type Pictures
    = NotLoaded
    | Loading
    | Loaded (List Picture)
    | Unavailable String


type alias Picture =
    { id : Int
    , url : String
    , job : Pipeline.Job
    , model : String
    }


type alias Stored =
    { id : Int
    , url : String
    , prompt : String
    , width : Int
    , height : Int
    , steps : Int
    , seed : Int
    , model : String
    }


type Save
    = Unsaved
    | Saving
    | Saved


type Notice
    = Info String
    | Problem String


init : Bool -> Model
init canShare =
    { pictures = NotLoaded
    , index = Bm25.empty
    , query = ""
    , viewing = Nothing
    , zoomed = False
    , detailed = False
    , confirming = Nothing
    , saves = []
    , notice = Nothing
    , canShare = canShare
    }


fromStored : Stored -> Maybe Picture
fromStored stored =
    Maybe.map5
        (\prompt width height steps seed ->
            { id = stored.id
            , url = stored.url
            , job = { prompt = prompt, width = width, height = height, steps = steps, seed = seed }
            , model = stored.model
            }
        )
        (Prompt.parse stored.prompt)
        (Dimension.fromInt stored.width)
        (Dimension.fromInt stored.height)
        (Steps.fromInt stored.steps)
        (Seed.fromInt stored.seed)


modelName : Picture -> String
modelName picture =
    Pipeline.weightsFromId picture.model
        |> Maybe.map Pipeline.weightsName
        |> Maybe.withDefault picture.model


fileName : Pipeline.Job -> String
fileName job =
    "pixo-" ++ Seed.toString job.seed ++ ".png"


saveOf : String -> Model -> Save
saveOf url model =
    List.filter (\( source, _ ) -> source == url) model.saves
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault Unsaved


find : Int -> Model -> Maybe Picture
find id model =
    case model.pictures of
        Loaded pictures ->
            List.filter (\picture -> picture.id == id) pictures
                |> List.head

        _ ->
            Nothing


visible : Model -> List Picture
visible model =
    case model.pictures of
        Loaded pictures ->
            if List.isEmpty (Bm25.tokens model.query) then
                pictures

            else
                Bm25.search model.query model.index
                    |> List.filterMap (\id -> find id model)

        _ ->
            []



-- UPDATE


type Msg
    = QueryChanged String
    | Opened Int
    | Closed
    | DetailsToggled
    | DeleteRequested Int
    | DeleteCancelled
    | DeleteConfirmed Int
    | SaveClicked Pipeline.Picture
    | ShareClicked Int
    | CopyClicked Int
    | PostClicked Share.Network Int
    | NoticeDismissed
    | Happened Event


type Event
    = PicturesLoaded (List Stored)
    | PictureSaved String Stored
    | PictureDeleted Int
    | Failed String
    | Copied
    | ShareFailed String


type Command
    = Load
    | Save Pipeline.Picture
    | Delete Picture
    | Share Picture
    | Copy Picture
    | Open String


enter : Model -> ( Model, List Command )
enter model =
    case model.pictures of
        NotLoaded ->
            ( { model | pictures = Loading }, [ Load ] )

        Unavailable _ ->
            ( { model | pictures = Loading }, [ Load ] )

        _ ->
            ( model, [] )


update : Msg -> Model -> ( Model, List Command )
update msg model =
    case msg of
        QueryChanged query ->
            ( { model | query = query }, [] )

        Opened id ->
            ( { model | viewing = Just id, zoomed = True, detailed = False, confirming = Nothing, notice = Nothing }, [] )

        Closed ->
            ( { model | zoomed = False, confirming = Nothing }, [] )

        DetailsToggled ->
            ( { model | detailed = not model.detailed }, [] )

        DeleteRequested id ->
            ( { model | confirming = Just id }, [] )

        DeleteCancelled ->
            ( { model | confirming = Nothing }, [] )

        DeleteConfirmed id ->
            ( { model | confirming = Nothing }, with id model Delete )

        SaveClicked picture ->
            if saveOf picture.url model == Unsaved then
                ( { model | saves = ( picture.url, Saving ) :: model.saves, notice = Nothing }, [ Save picture ] )

            else
                ( model, [] )

        ShareClicked id ->
            ( { model | notice = Nothing }, with id model Share )

        CopyClicked id ->
            ( { model | notice = Nothing }, with id model Copy )

        PostClicked network id ->
            ( { model | notice = Nothing }
            , List.concatMap
                (\picture -> [ Copy picture, Open (Share.compose network (Prompt.toString picture.job.prompt)) ])
                (List.filterMap identity [ find id model ])
            )

        NoticeDismissed ->
            ( { model | notice = Nothing }, [] )

        Happened event ->
            happened event model


with : Int -> Model -> (Picture -> Command) -> List Command
with id model toCommand =
    List.filterMap identity [ Maybe.map toCommand (find id model) ]


happened : Event -> Model -> ( Model, List Command )
happened event model =
    case event of
        PicturesLoaded stored ->
            ( shelve (List.filterMap fromStored stored) model, [] )

        PictureSaved source stored ->
            let
                saved =
                    { model | saves = ( source, Saved ) :: List.filter (\( known, _ ) -> known /= source) model.saves }
            in
            case ( model.pictures, fromStored stored ) of
                ( Loaded pictures, Just picture ) ->
                    ( shelve (picture :: pictures) saved, [] )

                ( Loading, _ ) ->
                    ( saved, [ Load ] )

                _ ->
                    ( saved, [] )

        PictureDeleted id ->
            case model.pictures of
                Loaded pictures ->
                    ( shelve (List.filter (\picture -> picture.id /= id) pictures)
                        { model | zoomed = model.zoomed && model.viewing /= Just id }
                    , []
                    )

                _ ->
                    ( model, [] )

        Failed reason ->
            ( { model
                | pictures =
                    if model.pictures == Loading then
                        Unavailable reason

                    else
                        model.pictures
                , saves = List.filter (\( _, save ) -> save /= Saving) model.saves
                , notice =
                    if model.pictures == Loading then
                        Nothing

                    else
                        Just (Problem reason)
              }
            , []
            )

        Copied ->
            ( { model | notice = Just (Info "The picture is copied. Paste it into your post.") }, [] )

        ShareFailed "AbortError" ->
            ( model, [] )

        ShareFailed _ ->
            ( { model | notice = Just (Problem "This browser couldn't hand the picture over. Download it instead.") }, [] )


shelve : List Picture -> Model -> Model
shelve pictures model =
    let
        newestFirst =
            List.sortBy (.id >> negate) pictures
    in
    { model
        | pictures = Loaded newestFirst
        , index = Bm25.fromList (List.map (\picture -> ( picture.id, Prompt.toString picture.job.prompt )) newestFirst)
    }
