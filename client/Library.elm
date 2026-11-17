module Library exposing
    ( Command(..)
    , Event(..)
    , Model
    , Msg(..)
    , Picture
    , Pictures(..)
    , Save(..)
    , chosen
    , cover
    , enter
    , fileName
    , find
    , init
    , markPublished
    , modelName
    , ownerOf
    , saveOf
    , saved
    , signedIn
    , signedOut
    , tags
    , untagged
    , update
    , visible
    )

import Bm25
import Data.Dimension as Dimension
import Data.PictureId as PictureId exposing (PictureId)
import Data.Prompt as Prompt
import Data.Seed as Seed
import Data.Steps as Steps
import Data.Tag as Tag exposing (Tag)
import Data.Username as Username exposing (Username)
import Dict
import Idb
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Notice exposing (Notice)
import Pipeline
import Share



-- MODEL


type alias Model =
    { pictures : Pictures
    , index : Bm25.Index
    , query : String
    , tag : Maybe Tag
    , allTags : Bool
    , viewing : Maybe Int
    , zoomed : Bool
    , detailed : Bool
    , confirming : Maybe Int
    , saves : List ( String, Save )
    , notice : Maybe (Notice Msg)
    , canShare : Bool
    , owner : Maybe Username
    }


type Pictures
    = NotLoaded
    | Loading
    | Loaded (List Picture)
    | Unavailable


type alias Picture =
    { id : Int
    , url : String
    , job : Pipeline.Job
    , model : String
    , tags : Maybe (List Tag)
    , published : Maybe PictureId
    }


type alias Row =
    { stored : Stored
    , tags : Maybe (List String)
    , owner : Maybe String
    , published : Maybe String
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


init : Bool -> Model
init canShare =
    { pictures = NotLoaded
    , index = Bm25.empty
    , query = ""
    , tag = Nothing
    , allTags = False
    , viewing = Nothing
    , zoomed = False
    , detailed = False
    , confirming = Nothing
    , saves = []
    , notice = Nothing
    , canShare = canShare
    , owner = Nothing
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


tagging : String
tagging =
    "tags"


claiming : String
claiming =
    "owner"


publishing : String
publishing =
    "published"


load : Command
load =
    Database (Idb.GetAll { database = database, store = store, label = "" })


save : Username -> Pipeline.Picture -> Command
save name picture =
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
                , ( "owner", Encode.string (Username.toString name) )
                ]
            , blobs = [ ( "blob", picture.url ) ]
            , label = picture.url
            }
        )


delete : Picture -> Command
delete picture =
    Database (Idb.Delete { database = database, store = store, key = Encode.int picture.id, label = "" })


read : Value -> Maybe Picture
read =
    readOwned >> Maybe.map Tuple.first


readOwned : Value -> Maybe ( Picture, Maybe String )
readOwned row =
    Decode.decodeValue decoder row
        |> Result.toMaybe
        |> Maybe.andThen
            (\found ->
                fromStored found.stored
                    |> Maybe.map
                        (\picture ->
                            ( { picture
                                | tags = Maybe.map (List.filterMap Tag.fromString) found.tags
                                , published = Maybe.andThen PictureId.fromString found.published
                              }
                            , found.owner
                            )
                        )
            )


saved : Event -> Maybe { id : Int, prompt : String }
saved event =
    case event of
        Answered (Idb.Added { row }) ->
            read row
                |> Maybe.map (\picture -> { id = picture.id, prompt = Prompt.toString picture.job.prompt })

        _ ->
            Nothing


decoder : Decoder Row
decoder =
    Decode.map4 Row
        (Decode.map8 Stored
            (Decode.field "id" Decode.int)
            (Decode.field "blob" Decode.string)
            (Decode.field "prompt" Decode.string)
            (Decode.field "width" Decode.int)
            (Decode.field "height" Decode.int)
            (Decode.field "steps" Decode.int)
            (Decode.field "seed" Decode.int)
            (Decode.field "model" Decode.string)
        )
        (Decode.maybe (Decode.field "tags" (Decode.list Decode.string)))
        (Decode.maybe (Decode.field "owner" Decode.string))
        (Decode.maybe (Decode.field "published" Decode.string))


fromStored : Stored -> Maybe Picture
fromStored stored =
    Maybe.map5
        (\prompt width height steps seed ->
            { id = stored.id
            , url = stored.url
            , job = { prompt = prompt, width = width, height = height, steps = steps, seed = seed }
            , model = stored.model
            , tags = Nothing
            , published = Nothing
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
            let
                found =
                    if List.isEmpty (Bm25.tokens model.query) then
                        pictures

                    else
                        Bm25.search model.query model.index
                            |> List.filterMap (\id -> find id model)
            in
            case chosen model of
                Just tag ->
                    List.filter (carries tag) found

                Nothing ->
                    found

        _ ->
            []


carries : Tag -> Picture -> Bool
carries tag picture =
    List.member tag (Maybe.withDefault [] picture.tags)


cover : Maybe Tag -> Model -> Maybe Picture
cover tag model =
    case model.pictures of
        Loaded pictures ->
            case tag of
                Just wanted ->
                    List.head (List.filter (carries wanted) pictures)

                Nothing ->
                    List.head pictures

        _ ->
            Nothing


tags : Model -> List ( Tag, Int )
tags model =
    case model.pictures of
        Loaded pictures ->
            pictures
                |> List.foldl
                    (\picture counts ->
                        List.foldl
                            (\tag -> Dict.update (Tag.toString tag) (countOne tag))
                            counts
                            (distinct (Maybe.withDefault [] picture.tags))
                    )
                    Dict.empty
                |> Dict.values
                |> List.sortBy (\( tag, n ) -> ( negate n, Tag.toString tag ))

        _ ->
            []


untagged : Model -> List { id : Int, prompt : String }
untagged model =
    case model.pictures of
        Loaded pictures ->
            pictures
                |> List.filter (\picture -> picture.tags == Nothing)
                |> List.map (\picture -> { id = picture.id, prompt = Prompt.toString picture.job.prompt })

        _ ->
            []


countOne : Tag -> Maybe ( Tag, Int ) -> Maybe ( Tag, Int )
countOne tag known =
    case known of
        Just ( _, n ) ->
            Just ( tag, n + 1 )

        Nothing ->
            Just ( tag, 1 )


distinct : List Tag -> List Tag
distinct all =
    all
        |> List.map (\tag -> ( Tag.toString tag, tag ))
        |> Dict.fromList
        |> Dict.values


chosen : Model -> Maybe Tag
chosen model =
    model.tag
        |> Maybe.andThen
            (\tag ->
                if List.any (\( known, _ ) -> known == tag) (tags model) then
                    Just tag

                else
                    Nothing
            )



-- UPDATE


type Msg
    = QueryChanged String
    | TagChosen (Maybe Tag)
    | TagsToggled
    | Opened Int
    | Closed
    | Stepped Int
    | DetailsToggled
    | DeleteRequested Int
    | DeleteCancelled
    | DeleteConfirmed Int
    | SaveClicked Pipeline.Picture
    | Tagged Int (List Tag)
    | ShareClicked Int
    | CopyClicked Int
    | PostClicked Share.Network Int
    | LoadRetried
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
    if model.owner == Nothing then
        ( model, [] )

    else
        case model.pictures of
            NotLoaded ->
                ( { model | pictures = Loading }, [ load ] )

            Unavailable ->
                ( { model | pictures = Loading }, [ load ] )

            _ ->
                ( model, [] )


signedIn : Username -> Model -> ( Model, List Command )
signedIn name model =
    if model.owner == Just name then
        ( model, [] )

    else
        let
            ( fresh, released ) =
                signedOut model
        in
        ( { fresh | owner = Just name }, released )


signedOut : Model -> ( Model, List Command )
signedOut model =
    ( init model.canShare
    , case model.pictures of
        Loaded ((_ :: _) as pictures) ->
            [ Database (Idb.Release (List.map .url pictures)) ]

        _ ->
            []
    )


ownerOf : Model -> Maybe Username
ownerOf model =
    model.owner


markPublished : Int -> Maybe PictureId -> Model -> ( Model, List Command )
markPublished id published model =
    case model.pictures of
        Loaded pictures ->
            ( shelve
                (List.map
                    (\picture ->
                        if picture.id == id then
                            { picture | published = published }

                        else
                            picture
                    )
                    pictures
                )
                model
            , [ Database
                    (Idb.Patch
                        { database = database
                        , store = store
                        , key = Encode.int id
                        , fields = [ ( "published", Maybe.withDefault Encode.null (Maybe.map PictureId.encode published) ) ]
                        , label = publishing
                        }
                    )
              ]
            )

        _ ->
            ( model, [] )


update : Msg -> Model -> ( Model, List Command )
update msg model =
    case msg of
        QueryChanged query ->
            ( { model | query = query }, [] )

        TagChosen tag ->
            ( { model | tag = tag, confirming = Nothing }, [] )

        TagsToggled ->
            ( { model | allTags = not model.allTags }, [] )

        Opened id ->
            ( { model | viewing = Just id, zoomed = True, detailed = False, confirming = Nothing }, [] )

        Closed ->
            ( { model | zoomed = False, confirming = Nothing }, [] )

        Stepped offset ->
            case Maybe.andThen (\viewed -> neighbour offset viewed (List.map .id (visible model))) model.viewing of
                Just id ->
                    if model.zoomed then
                        ( { model | viewing = Just id, confirming = Nothing }, [] )

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
            case model.owner of
                Just name ->
                    if saveOf picture.url model == Unsaved then
                        ( { model | saves = ( picture.url, Saving ) :: model.saves }, [ save name picture ] )

                    else
                        ( model, [] )

                Nothing ->
                    ( model, [] )

        Tagged id names ->
            ( case model.pictures of
                Loaded pictures ->
                    shelve
                        (List.map
                            (\picture ->
                                if picture.id == id then
                                    { picture | tags = Just names }

                                else
                                    picture
                            )
                            pictures
                        )
                        model

                _ ->
                    model
            , [ Database
                    (Idb.Patch
                        { database = database
                        , store = store
                        , key = Encode.int id
                        , fields = [ ( "tags", Encode.list (Tag.toString >> Encode.string) names ) ]
                        , label = tagging
                        }
                    )
              ]
            )

        ShareClicked id ->
            ( model, with id model Share )

        CopyClicked id ->
            ( model, with id model Copy )

        PostClicked network id ->
            ( model
            , List.concatMap
                (\picture -> [ Copy picture, Open (Share.compose network (Prompt.toString picture.job.prompt)) ])
                (List.filterMap identity [ find id model ])
            )

        LoadRetried ->
            enter model

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
            case model.owner of
                Just name ->
                    let
                        mine =
                            Username.toString name

                        owned =
                            List.filterMap readOwned rows
                                |> List.filter (\( _, owner ) -> owner == Nothing || owner == Just mine)
                    in
                    ( shelve (List.map Tuple.first owned) model
                    , owned
                        |> List.filter (\( _, owner ) -> owner == Nothing)
                        |> List.map (\( picture, _ ) -> claim mine picture.id)
                    )

                Nothing ->
                    ( shelve [] model, [] )

        Answered (Idb.Added { label, row }) ->
            let
                stored =
                    { model | saves = ( label, Saved ) :: List.filter (\( known, _ ) -> known /= label) model.saves }
            in
            case ( model.pictures, read row ) of
                ( Loaded pictures, Just picture ) ->
                    ( shelve (picture :: pictures) stored, [] )

                ( Loading, _ ) ->
                    ( stored, [ load ] )

                _ ->
                    ( stored, [] )

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

        Answered (Idb.Patched _) ->
            ( model, [] )

        Answered (Idb.Failed { label, message }) ->
            if label == tagging || label == claiming || label == publishing then
                ( model, [] )

            else if model.pictures == Loading then
                ( { model
                    | pictures = Unavailable
                    , saves = List.filter (\( _, state ) -> state /= Saving) model.saves
                    , notice = Just (storageNotice "Can't open the library" message (Notice.Retry LoadRetried))
                  }
                , []
                )

            else
                ( { model
                    | saves = List.filter (\( _, state ) -> state /= Saving) model.saves
                    , notice =
                        Just
                            (storageNotice
                                (if List.member ( label, Saving ) model.saves then
                                    "Couldn't save the picture"

                                 else
                                    "Couldn't delete the picture"
                                )
                                message
                                Notice.NoRetry
                            )
                  }
                , []
                )

        Copied ->
            ( { model | notice = Just { id = "share", tone = Notice.Success, title = "Picture copied", description = Nothing, retry = Notice.NoRetry } }, [] )

        ShareFailed "AbortError" ->
            ( model, [] )

        ShareFailed _ ->
            ( { model | notice = Just { id = "share", tone = Notice.Problem, title = "Couldn't share the picture", description = Just "Download it instead.", retry = Notice.NoRetry } }, [] )


claim : String -> Int -> Command
claim mine id =
    Database
        (Idb.Patch
            { database = database
            , store = store
            , key = Encode.int id
            , fields = [ ( "owner", Encode.string mine ) ]
            , label = claiming
            }
        )


storageNotice : String -> String -> Notice.Retry Msg -> Notice Msg
storageNotice title message retry =
    { id = "library", tone = Notice.Problem, title = title, description = Just message, retry = retry }


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
