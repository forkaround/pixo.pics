module Library exposing
    ( Command(..)
    , Event(..)
    , Model
    , Msg(..)
    , Notice(..)
    , Picture
    , Pictures(..)
    , Save(..)
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
import Idb
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
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


database : Idb.Database
database =
    { name = "pixo"
    , version = 1
    , stores = [ { name = store, keyPath = "id", autoIncrement = True } ]
    }


store : String
store =
    "pictures"


load : Command
load =
    Database (Idb.GetAll { database = database, store = store, label = "" })


save : Pipeline.Picture -> Command
save picture =
    Database
        (Idb.Add
            { database = database
            , store = store
            , row =
                [ ( "prompt", Encode.string (Prompt.toString picture.job.prompt) )
                , ( "width", Encode.int (Dimension.toInt picture.job.width) )
                , ( "height", Encode.int (Dimension.toInt picture.job.height) )
                , ( "steps", Encode.int (Steps.toInt picture.job.steps) )
                , ( "seed", Encode.int (Seed.toInt picture.job.seed) )
                , ( "model", Encode.string (Pipeline.weightsId picture.weights) )
                ]
            , blobs = [ ( "blob", picture.url ) ]
            , label = picture.url
            }
        )


delete : Picture -> Command
delete picture =
    Database (Idb.Delete { database = database, store = store, key = Encode.int picture.id, label = "" })


read : Value -> Maybe Picture
read row =
    Decode.decodeValue decoder row
        |> Result.toMaybe
        |> Maybe.andThen fromStored


decoder : Decoder Stored
decoder =
    Decode.map8 Stored
        (Decode.field "id" Decode.int)
        (Decode.field "blob" Decode.string)
        (Decode.field "prompt" Decode.string)
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)
        (Decode.field "steps" Decode.int)
        (Decode.field "seed" Decode.int)
        (Decode.field "model" Decode.string)


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
    | Stepped Int
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
    = Answered Idb.Event
    | Copied
    | ShareFailed String


type Command
    = Database Idb.Command
    | Share Picture
    | Copy Picture
    | Open String


enter : Model -> ( Model, List Command )
enter model =
    case model.pictures of
        NotLoaded ->
            ( { model | pictures = Loading }, [ load ] )

        Unavailable _ ->
            ( { model | pictures = Loading }, [ load ] )

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

        Stepped offset ->
            case Maybe.andThen (\viewed -> neighbour offset viewed (List.map .id (visible model))) model.viewing of
                Just id ->
                    if model.zoomed then
                        ( { model | viewing = Just id, confirming = Nothing, notice = Nothing }, [] )

                    else
                        ( model, [] )

                Nothing ->
                    ( model, [] )

        DetailsToggled ->
            ( { model | detailed = not model.detailed }, [] )

        DeleteRequested id ->
            ( { model | confirming = Just id }, [] )

        DeleteCancelled ->
            ( { model | confirming = Nothing }, [] )

        DeleteConfirmed id ->
            ( { model | confirming = Nothing }, with id model delete )

        SaveClicked picture ->
            if saveOf picture.url model == Unsaved then
                ( { model | saves = ( picture.url, Saving ) :: model.saves, notice = Nothing }, [ save picture ] )

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


neighbour : Int -> a -> List a -> Maybe a
neighbour offset key keys =
    keys
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, known ) -> known == key)
        |> List.head
        |> Maybe.map (\( index, _ ) -> index + offset)
        |> Maybe.andThen
            (\index ->
                if index < 0 then
                    Nothing

                else
                    List.head (List.drop index keys)
            )


with : Int -> Model -> (Picture -> Command) -> List Command
with id model toCommand =
    List.filterMap identity [ Maybe.map toCommand (find id model) ]


happened : Event -> Model -> ( Model, List Command )
happened event model =
    case event of
        Answered (Idb.Rows { rows }) ->
            ( shelve (List.filterMap read rows) model, [] )

        Answered (Idb.Added { label, row }) ->
            let
                saved =
                    { model | saves = ( label, Saved ) :: List.filter (\( known, _ ) -> known /= label) model.saves }
            in
            case ( model.pictures, read row ) of
                ( Loaded pictures, Just picture ) ->
                    ( shelve (picture :: pictures) saved, [] )

                ( Loading, _ ) ->
                    ( saved, [ load ] )

                _ ->
                    ( saved, [] )

        Answered (Idb.Deleted { key }) ->
            case ( model.pictures, Decode.decodeValue Decode.int key ) of
                ( Loaded pictures, Ok id ) ->
                    let
                        ( gone, kept ) =
                            List.partition (\picture -> picture.id == id) pictures
                    in
                    ( shelve kept { model | zoomed = model.zoomed && model.viewing /= Just id }
                    , [ Database (Idb.Release (List.map .url gone)) ]
                    )

                _ ->
                    ( model, [] )

        Answered (Idb.Failed { message }) ->
            ( { model
                | pictures =
                    if model.pictures == Loading then
                        Unavailable message

                    else
                        model.pictures
                , saves = List.filter (\( _, state ) -> state /= Saving) model.saves
                , notice =
                    if model.pictures == Loading then
                        Nothing

                    else
                        Just (Problem message)
              }
            , []
            )

        Copied ->
            ( { model | notice = Just (Info "Picture copied.") }, [] )

        ShareFailed "AbortError" ->
            ( model, [] )

        ShareFailed _ ->
            ( { model | notice = Just (Problem "Couldn't share the picture. Download it instead.") }, [] )


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
