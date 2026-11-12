module Pipeline exposing
    ( Command(..)
    , Component(..)
    , Download
    , Engine(..)
    , Event(..)
    , Failure
    , Job
    , Message(..)
    , Model
    , Msg(..)
    , Pending
    , Picture
    , Preset
    , Seeding(..)
    , Settings
    , Stage(..)
    , Weights(..)
    , current
    , enter
    , frameBudget
    , guidance
    , inMemory
    , init
    , isDownloaded
    , presets
    , remember
    , remembered
    , update
    , weights
    , weightsFromId
    , weightsId
    , weightsName
    , weightsSize
    )

import Data.Dimension as Dimension exposing (Dimension)
import Data.Prompt as Prompt exposing (Prompt)
import Data.Seed as Seed exposing (Seed)
import Data.Steps as Steps exposing (Steps)
import Json.Decode as Decode
import Json.Encode as Encode
import Random



-- MODEL


type alias Model =
    { engine : Engine
    , settings : Settings
    , message : Maybe Message
    , pictures : List Picture
    , detailed : Maybe String
    , downloaded : List Weights
    , random : Random.Seed
    }


type Engine
    = Unchecked
    | Checking
    | Unsupported
    | Empty
    | Clearing
    | Loading Weights (List Download) (Maybe Job)
    | Ready Weights
    | Generating Weights Job Stage
    | Releasing (Maybe Pending)


type alias Pending =
    { weights : Weights
    , job : Maybe Job
    }


type Stage
    = Denoising Int
    | Stopping


type Message
    = Problem String
    | Info String


type Weights
    = Ternary
    | Binary


type alias Settings =
    { weights : Weights
    , prompt : String
    , width : Dimension
    , height : Dimension
    , steps : Steps
    , seeding : Seeding
    }


type Seeding
    = RandomSeed
    | FixedSeed String


type alias Job =
    { prompt : Prompt
    , width : Dimension
    , height : Dimension
    , steps : Steps
    , seed : Seed
    }


type alias Picture =
    { url : String
    , job : Job
    , weights : Weights
    }


type alias Preset =
    { name : String
    , ratio : String
    , use : String
    , width : Dimension
    , height : Dimension
    }


type Component
    = TextEncoder
    | Transformer
    | Vae


type alias Download =
    { component : Component
    , loaded : Int
    , total : Int
    , cached : Bool
    }


type alias Failure =
    { name : String
    , message : String
    }


init : Int -> List Weights -> Model
init entropy downloaded =
    { engine = Unchecked
    , settings =
        { weights = Maybe.withDefault Ternary (List.head downloaded)
        , prompt = ""
        , width = Dimension.default
        , height = Dimension.default
        , steps = Steps.default
        , seeding = RandomSeed
        }
    , message = Nothing
    , pictures = []
    , detailed = Nothing
    , downloaded = downloaded
    , random = Random.initialSeed entropy
    }


weights : List Weights
weights =
    [ Ternary, Binary ]


weightsId : Weights -> String
weightsId chosen =
    case chosen of
        Ternary ->
            "prism-ml/bonsai-image-ternary-4B-mlx-2bit"

        Binary ->
            "prism-ml/bonsai-image-binary-4B-mlx-1bit"


weightsFromId : String -> Maybe Weights
weightsFromId id =
    List.filter (\known -> weightsId known == id) weights
        |> List.head


weightsName : Weights -> String
weightsName chosen =
    case chosen of
        Ternary ->
            "Bonsai ternary, 2-bit"

        Binary ->
            "Bonsai binary, 1-bit"


weightsSize : Weights -> String
weightsSize chosen =
    case chosen of
        Ternary ->
            "3.4 GB"

        Binary ->
            "2.9 GB"


presets : List Preset
presets =
    List.filterMap identity
        [ preset "Square" "1:1" "Instagram post, profile picture" 1024 1024
        , preset "Portrait" "4:5" "Instagram portrait" 768 960
        , preset "Story" "9:16" "Stories, Reels, TikTok, Shorts" 576 1024
        , preset "Widescreen" "16:9" "YouTube thumbnail, X post" 1024 576
        , preset "Pin" "2:3" "Pinterest pin" 640 960
        , preset "Link card" "2:1" "Link preview on X, LinkedIn, Facebook" 1024 512
        , preset "Banner" "3:1" "X header" 960 320
        ]


preset : String -> String -> String -> Int -> Int -> Maybe Preset
preset name ratio use width height =
    Maybe.map2 (Preset name ratio use) (Dimension.fromInt width) (Dimension.fromInt height)


isDownloaded : Weights -> Model -> Bool
isDownloaded chosen model =
    List.member chosen model.downloaded


inMemory : Engine -> Maybe Weights
inMemory engine =
    case engine of
        Ready loaded ->
            Just loaded

        Generating loaded _ _ ->
            Just loaded

        _ ->
            Nothing


remember : List Weights -> String
remember downloaded =
    Encode.encode 0 (Encode.list (weightsId >> Encode.string) downloaded)


remembered : String -> List Weights
remembered stored =
    Decode.decodeString (Decode.list Decode.string) stored
        |> Result.withDefault []
        |> List.filterMap weightsFromId


current : List Download -> Maybe Download
current downloads =
    List.head downloads


guidance : Float
guidance =
    1


frameBudget : Int
frameBudget =
    8



-- UPDATE


type Msg
    = WeightsChosen Weights
    | DownloadClicked Weights
    | CancelClicked
    | ReleaseClicked
    | ClearClicked
    | PromptChanged String
    | PresetChosen Preset
    | WidthChanged Float
    | HeightChanged Float
    | StepsChanged Float
    | RandomSeedToggled Bool
    | SeedChanged String
    | SeedReused Seed
    | DetailsToggled String
    | GenerateClicked
    | Happened Event


type Event
    = SupportChecked Bool
    | Downloaded Download
    | ModelLoaded
    | StepFinished Int
    | PictureMade String
    | ModelReleased
    | CacheCleared
    | Failed Failure


type Command
    = Check
    | Load Weights
    | Generate Job
    | Cancel
    | Release
    | ClearCache


enter : Model -> ( Model, Maybe Command )
enter model =
    case model.engine of
        Unchecked ->
            ( { model | engine = Checking }, Just Check )

        _ ->
            ( model, Nothing )


update : Msg -> Model -> ( Model, Maybe Command )
update msg model =
    case msg of
        WeightsChosen chosen ->
            ( edit (\settings -> { settings | weights = chosen }) model, Nothing )

        PromptChanged prompt ->
            ( edit (\settings -> { settings | prompt = prompt }) model, Nothing )

        PresetChosen chosen ->
            ( edit (\settings -> { settings | width = chosen.width, height = chosen.height }) model, Nothing )

        WidthChanged pixels ->
            ( edit (\settings -> { settings | width = Maybe.withDefault settings.width (Dimension.fromInt (round pixels)) }) model, Nothing )

        HeightChanged pixels ->
            ( edit (\settings -> { settings | height = Maybe.withDefault settings.height (Dimension.fromInt (round pixels)) }) model, Nothing )

        StepsChanged count ->
            ( edit (\settings -> { settings | steps = Maybe.withDefault settings.steps (Steps.fromInt (round count)) }) model, Nothing )

        RandomSeedToggled True ->
            ( edit (\settings -> { settings | seeding = RandomSeed }) model, Nothing )

        RandomSeedToggled False ->
            ( edit (\settings -> { settings | seeding = FixedSeed (lastSeed model.pictures) }) model, Nothing )

        SeedChanged seed ->
            ( edit (\settings -> { settings | seeding = FixedSeed seed }) model, Nothing )

        DetailsToggled url ->
            ( { model
                | detailed =
                    if model.detailed == Just url then
                        Nothing

                    else
                        Just url
              }
            , Nothing
            )

        SeedReused seed ->
            ( edit (\settings -> { settings | seeding = FixedSeed (Seed.toString seed) }) model, Nothing )

        DownloadClicked chosen ->
            start { weights = chosen, job = Nothing } model

        CancelClicked ->
            case model.engine of
                Loading _ _ _ ->
                    ( model, Just Cancel )

                Generating loaded job (Denoising _) ->
                    ( { model | engine = Generating loaded job Stopping }, Just Cancel )

                _ ->
                    ( model, Nothing )

        ReleaseClicked ->
            case model.engine of
                Ready _ ->
                    ( { model | engine = Releasing Nothing, message = Nothing }, Just Release )

                _ ->
                    ( model, Nothing )

        ClearClicked ->
            case model.engine of
                Empty ->
                    ( { model | engine = Clearing, message = Nothing }, Just ClearCache )

                _ ->
                    ( model, Nothing )

        GenerateClicked ->
            if inMemory model.engine == Nothing && model.engine /= Empty then
                ( model, Nothing )

            else
                case plan model of
                    Ok ( job, random ) ->
                        start { weights = model.settings.weights, job = Just job } { model | random = random }

                    Err problem ->
                        ( { model | message = Just (Problem problem) }, Nothing )

        Happened event ->
            happened event model


start : Pending -> Model -> ( Model, Maybe Command )
start pending model =
    case model.engine of
        Empty ->
            load pending model

        Ready loaded ->
            if loaded /= pending.weights then
                ( { model | engine = Releasing (Just pending), message = Nothing }, Just Release )

            else
                case pending.job of
                    Just job ->
                        generate loaded job model

                    Nothing ->
                        ( model, Nothing )

        _ ->
            ( model, Nothing )


load : Pending -> Model -> ( Model, Maybe Command )
load pending model =
    ( { model | engine = Loading pending.weights [] pending.job, message = Nothing }
    , Just (Load pending.weights)
    )


generate : Weights -> Job -> Model -> ( Model, Maybe Command )
generate loaded job model =
    ( { model | engine = Generating loaded job (Denoising 0), message = Nothing }, Just (Generate job) )


happened : Event -> Model -> ( Model, Maybe Command )
happened event model =
    case ( event, model.engine ) of
        ( SupportChecked True, Checking ) ->
            ( { model | engine = Empty }, Nothing )

        ( SupportChecked False, Checking ) ->
            ( { model | engine = Unsupported }, Nothing )

        ( Downloaded progress, Loading chosen downloads waiting ) ->
            ( { model
                | engine =
                    Loading chosen
                        (progress :: List.filter (\known -> known.component /= progress.component) downloads)
                        waiting
              }
            , Nothing
            )

        ( ModelLoaded, Loading chosen _ waiting ) ->
            let
                loaded =
                    { model
                        | engine = Ready chosen
                        , downloaded = chosen :: List.filter (\known -> known /= chosen) model.downloaded
                    }
            in
            case waiting of
                Just job ->
                    generate chosen job loaded

                Nothing ->
                    ( loaded, Nothing )

        ( StepFinished index, Generating loaded job (Denoising _) ) ->
            ( { model | engine = Generating loaded job (Denoising (index + 1)) }, Nothing )

        ( PictureMade url, Generating loaded job _ ) ->
            ( { model | engine = Ready loaded, pictures = { url = url, job = job, weights = loaded } :: model.pictures }
            , Nothing
            )

        ( ModelReleased, Releasing (Just pending) ) ->
            load pending { model | engine = Empty }

        ( ModelReleased, Releasing Nothing ) ->
            ( { model | engine = Empty }, Nothing )

        ( CacheCleared, Clearing ) ->
            ( { model
                | engine = Empty
                , downloaded = []
                , message = Just (Info "Downloaded models removed.")
              }
            , Nothing
            )

        ( Failed failure, _ ) ->
            case rest model.engine of
                Just engine ->
                    ( { model
                        | engine = engine
                        , message =
                            if failure.name == "AbortError" && isCancellable model.engine then
                                Nothing

                            else
                                Just (Problem (explain failure))
                      }
                    , Nothing
                    )

                Nothing ->
                    ( model, Nothing )

        _ ->
            ( model, Nothing )


isCancellable : Engine -> Bool
isCancellable engine =
    case engine of
        Loading _ _ _ ->
            True

        Generating _ _ Stopping ->
            True

        _ ->
            False


rest : Engine -> Maybe Engine
rest engine =
    case engine of
        Unchecked ->
            Nothing

        Checking ->
            Just Unsupported

        Unsupported ->
            Nothing

        Empty ->
            Nothing

        Clearing ->
            Just Empty

        Loading _ _ _ ->
            Just Empty

        Ready _ ->
            Nothing

        Generating loaded _ _ ->
            Just (Ready loaded)

        Releasing _ ->
            Just Empty


explain : Failure -> String
explain failure =
    if String.isEmpty (String.trim failure.message) then
        "Something went wrong: " ++ failure.name ++ "."

    else
        failure.message


plan : Model -> Result String ( Job, Random.Seed )
plan { settings, random } =
    case ( Prompt.parse settings.prompt, draw settings.seeding random ) of
        ( Nothing, _ ) ->
            Err "Describe the picture first."

        ( _, Nothing ) ->
            Err ("The seed is a whole number from 0 to " ++ String.fromInt Seed.largest ++ ".")

        ( Just prompt, Just ( seed, next ) ) ->
            Ok
                ( { prompt = prompt
                  , width = settings.width
                  , height = settings.height
                  , steps = settings.steps
                  , seed = seed
                  }
                , next
                )


draw : Seeding -> Random.Seed -> Maybe ( Seed, Random.Seed )
draw seeding random =
    case seeding of
        RandomSeed ->
            Just (Random.step Seed.generator random)

        FixedSeed text ->
            Seed.fromString text
                |> Maybe.map (\seed -> ( seed, random ))


lastSeed : List Picture -> String
lastSeed pictures =
    List.head pictures
        |> Maybe.map (.job >> .seed >> Seed.toString)
        |> Maybe.withDefault "0"


edit : (Settings -> Settings) -> Model -> Model
edit change model =
    { model | settings = change model.settings }
