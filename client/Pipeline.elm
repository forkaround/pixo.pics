module Pipeline exposing
    ( Command(..)
    , Component(..)
    , Device(..)
    , Download
    , Draft(..)
    , Engine(..)
    , Event(..)
    , Failure
    , Job
    , Model
    , Msg(..)
    , NumberField(..)
    , Pending
    , Picture
    , Preset
    , Prompting(..)
    , Seeding(..)
    , Settings
    , Stage(..)
    , Storage(..)
    , Validation
    , Weights(..)
    , canPrompt
    , checks
    , current
    , engineOf
    , enter
    , frameBudget
    , guidance
    , inMemory
    , init
    , isDownloaded
    , isSupported
    , presets
    , prompting
    , remember
    , remembered
    , toast
    , update
    , weights
    , weightsFromId
    , weightsId
    , weightsName
    , weightsSize
    , weightsVariant
    )

import Data.Dimension as Dimension exposing (Dimension)
import Data.Prompt as Prompt exposing (Prompt)
import Data.Seed as Seed exposing (Seed)
import Data.Steps as Steps exposing (Steps)
import Json.Decode as Decode
import Json.Encode as Encode
import Notice exposing (Notice)
import Random



-- MODEL


type alias Model =
    { device : Device
    , settings : Settings
    , notice : Maybe (Notice Msg)
    , checked : Bool
    , pictures : List Picture
    , viewing : Maybe String
    , zoomed : Bool
    , detailed : Bool
    , downloaded : List Weights
    , random : Random.Seed
    }


type Device
    = Unchecked
    | Checking
    | Unsupported Storage
    | Supported Prompting Engine


type Prompting
    = CanPrompt
    | NoShaderF16


type Storage
    = Kept
    | Emptying
    | Unreachable


type Engine
    = Empty
    | Clearing
    | Loading Weights (List Download) (Maybe Job)
    | Cancelling Weights (List Download)
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


type alias Validation =
    { field : String
    , problem : String
    , failing : Bool
    }


type Weights
    = Ternary
    | Binary


type alias Settings =
    { weights : Weights
    , prompt : Draft
    , width : Dimension
    , height : Dimension
    , steps : String
    , seeding : Seeding
    , unreadable : List NumberField
    }


type NumberField
    = StepsField
    | SeedField


type Draft
    = Editing String
    | Styling


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


init : Int -> Bool -> List Weights -> Model
init entropy webGpu downloaded =
    { device =
        if webGpu then
            Unchecked

        else
            Unsupported Kept
    , settings =
        { weights = Maybe.withDefault Ternary (List.head downloaded)
        , prompt = Editing ""
        , width = Dimension.default
        , height = Dimension.default
        , steps = Steps.toString Steps.default
        , seeding = RandomSeed
        , unreadable = []
        }
    , notice = Nothing
    , checked = False
    , pictures = []
    , viewing = Nothing
    , zoomed = False
    , detailed = False
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


weightsVariant : Weights -> String
weightsVariant chosen =
    case chosen of
        Ternary ->
            "Ternary, 2-bit"

        Binary ->
            "Binary, 1-bit"


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


isSupported : Device -> Bool
isSupported device =
    case device of
        Supported _ _ ->
            True

        Unchecked ->
            False

        Checking ->
            False

        Unsupported _ ->
            False


promptingFrom : List String -> Prompting
promptingFrom features =
    if List.member "shader-f16" features then
        CanPrompt

    else
        NoShaderF16


engineOf : Device -> Maybe Engine
engineOf device =
    case device of
        Supported _ running ->
            Just running

        Unchecked ->
            Nothing

        Checking ->
            Nothing

        Unsupported _ ->
            Nothing


prompting : Device -> Maybe Prompting
prompting device =
    case device of
        Supported can _ ->
            Just can

        Unchecked ->
            Nothing

        Checking ->
            Nothing

        Unsupported _ ->
            Nothing


canPrompt : Device -> Bool
canPrompt device =
    prompting device == Just CanPrompt


setEngine : Engine -> Model -> Model
setEngine running model =
    case model.device of
        Supported can _ ->
            { model | device = Supported can running }

        Unchecked ->
            model

        Checking ->
            model

        Unsupported _ ->
            model


inMemory : Device -> Maybe Weights
inMemory device =
    case device of
        Supported _ (Ready loaded) ->
            Just loaded

        Supported _ (Generating loaded _ _) ->
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
    | StylingStarted
    | StylingEnded String
    | PresetChosen Preset
    | StepsChanged String
    | SeedChanged String
    | Unreadable NumberField
    | SeedReused Seed
    | Opened String
    | Closed
    | Stepped Int
    | DetailsToggled
    | GenerateClicked
    | Happened Event


type Event
    = SupportChecked (Maybe (List String))
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
    case model.device of
        Unchecked ->
            ( { model | device = Checking }, Just Check )

        _ ->
            ( model, Nothing )


update : Msg -> Model -> ( Model, Maybe Command )
update msg model =
    step msg model


step : Msg -> Model -> ( Model, Maybe Command )
step msg model =
    case msg of
        WeightsChosen chosen ->
            ( edit (\settings -> { settings | weights = chosen }) model, Nothing )

        PromptChanged prompt ->
            case model.settings.prompt of
                Editing _ ->
                    ( edit (\settings -> { settings | prompt = Editing prompt }) model, Nothing )

                Styling ->
                    ( model, Nothing )

        StylingStarted ->
            ( edit (\settings -> { settings | prompt = Styling }) model, Nothing )

        StylingEnded prompt ->
            ( edit (\settings -> { settings | prompt = Editing prompt }) model, Nothing )

        PresetChosen chosen ->
            ( edit (\settings -> { settings | width = chosen.width, height = chosen.height }) model, Nothing )

        StepsChanged count ->
            ( edit (\settings -> { settings | steps = count, unreadable = List.filter ((/=) StepsField) settings.unreadable }) model, Nothing )

        Unreadable field ->
            ( edit (\settings -> { settings | unreadable = field :: List.filter ((/=) field) settings.unreadable }) model, Nothing )

        SeedChanged seed ->
            ( edit
                (\settings ->
                    { settings
                        | unreadable = List.filter ((/=) SeedField) settings.unreadable
                        , seeding =
                            if String.isEmpty (String.trim seed) then
                                RandomSeed

                            else
                                FixedSeed seed
                    }
                )
                model
            , Nothing
            )

        Opened url ->
            ( { model | viewing = Just url, zoomed = True, detailed = False }, Nothing )

        Closed ->
            ( { model | zoomed = False }, Nothing )

        Stepped offset ->
            case Maybe.andThen (\viewed -> neighbour offset viewed (List.map .url model.pictures)) model.viewing of
                Just url ->
                    if model.zoomed then
                        ( { model | viewing = Just url }, Nothing )

                    else
                        ( model, Nothing )

                Nothing ->
                    ( model, Nothing )

        DetailsToggled ->
            ( { model | detailed = not model.detailed }, Nothing )

        SeedReused seed ->
            ( edit (\settings -> { settings | seeding = FixedSeed (Seed.toString seed), unreadable = List.filter ((/=) SeedField) settings.unreadable }) { model | zoomed = False }, Nothing )

        DownloadClicked chosen ->
            start { weights = chosen, job = Nothing } model

        CancelClicked ->
            case model.device of
                Supported _ (Loading chosen parts _) ->
                    ( setEngine (Cancelling chosen parts) model, Just Cancel )

                Supported _ (Generating loaded job (Denoising _)) ->
                    ( setEngine (Generating loaded job Stopping) model, Just Cancel )

                _ ->
                    ( model, Nothing )

        ReleaseClicked ->
            case model.device of
                Supported _ (Ready _) ->
                    ( setEngine (Releasing Nothing) model, Just Release )

                _ ->
                    ( model, Nothing )

        ClearClicked ->
            case model.device of
                Supported _ Empty ->
                    ( setEngine Clearing model, Just ClearCache )

                Unsupported Kept ->
                    ( { model | device = Unsupported Emptying }, Just ClearCache )

                _ ->
                    ( model, Nothing )

        GenerateClicked ->
            case model.settings.prompt of
                Styling ->
                    ( model, Nothing )

                Editing prompt ->
                    if inMemory model.device == Nothing && engineOf model.device /= Just Empty then
                        ( model, Nothing )

                    else
                        case plan prompt model of
                            Just ( job, random ) ->
                                start { weights = model.settings.weights, job = Just job } { model | random = random, checked = False }

                            Nothing ->
                                ( { model | checked = True }, Nothing )

        Happened event ->
            happened event model


start : Pending -> Model -> ( Model, Maybe Command )
start pending model =
    case model.device of
        Supported _ Empty ->
            load pending model

        Supported _ (Ready loaded) ->
            if loaded /= pending.weights then
                ( setEngine (Releasing (Just pending)) model, Just Release )

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
    ( setEngine (Loading pending.weights [] pending.job) model
    , Just (Load pending.weights)
    )


generate : Weights -> Job -> Model -> ( Model, Maybe Command )
generate loaded job model =
    ( setEngine (Generating loaded job (Denoising 0)) model, Just (Generate job) )


happened : Event -> Model -> ( Model, Maybe Command )
happened event model =
    case ( event, model.device ) of
        ( SupportChecked (Just features), Checking ) ->
            ( { model | device = Supported (promptingFrom features) Empty }, Nothing )

        ( CacheCleared, Unsupported Emptying ) ->
            ( { model
                | device = Unsupported Kept
                , downloaded = []
                , notice = Just removed
              }
            , Nothing
            )

        ( SupportChecked Nothing, Checking ) ->
            ( { model | device = Unsupported Kept }, Nothing )

        ( Downloaded progress, Supported _ (Loading chosen downloads waiting) ) ->
            ( setEngine
                (Loading chosen
                    (progress :: List.filter (\known -> known.component /= progress.component) downloads)
                    waiting
                )
                model
            , Nothing
            )

        ( ModelLoaded, Supported _ (Loading chosen _ waiting) ) ->
            let
                loaded =
                    setEngine (Ready chosen)
                        { model
                            | downloaded = chosen :: List.filter (\known -> known /= chosen) model.downloaded
                        }
            in
            case waiting of
                Just job ->
                    generate chosen job loaded

                Nothing ->
                    ( loaded, Nothing )

        ( ModelLoaded, Supported _ (Cancelling chosen _) ) ->
            ( setEngine (Ready chosen)
                { model
                    | downloaded = chosen :: List.filter (\known -> known /= chosen) model.downloaded
                }
            , Nothing
            )

        ( StepFinished index, Supported _ (Generating loaded job (Denoising _)) ) ->
            ( setEngine (Generating loaded job (Denoising (index + 1))) model, Nothing )

        ( PictureMade url, Supported _ (Generating loaded job _) ) ->
            ( setEngine (Ready loaded) { model | pictures = { url = url, job = job, weights = loaded } :: model.pictures }
            , Nothing
            )

        ( ModelReleased, Supported _ (Releasing (Just pending)) ) ->
            load pending (setEngine Empty model)

        ( ModelReleased, Supported _ (Releasing Nothing) ) ->
            ( setEngine Empty model, Nothing )

        ( CacheCleared, Supported _ Clearing ) ->
            ( setEngine Empty
                { model
                    | downloaded = []
                    , notice = Just removed
                }
            , Nothing
            )

        ( Failed failure, _ ) ->
            case rest model.device of
                Just device ->
                    ( { model
                        | device = device
                        , notice =
                            if failure.name == "AbortError" && isCancellable model.device then
                                Nothing

                            else
                                Just (failedNotice model.device failure)
                      }
                    , Nothing
                    )

                Nothing ->
                    ( model, Nothing )

        _ ->
            ( model, Nothing )


toast : String
toast =
    "pipeline"


removed : Notice Msg
removed =
    { id = toast, tone = Notice.Success, title = "Downloaded models removed", description = Nothing, retry = Notice.NoRetry }


failedNotice : Device -> Failure -> Notice Msg
failedNotice device failure =
    { id = toast
    , tone = Notice.Problem
    , title = failedTitle device
    , description = Just (explain failure)
    , retry = failedRetry device
    }


failedTitle : Device -> String
failedTitle device =
    case device of
        Supported _ (Loading _ _ _) ->
            "Couldn't download the model"

        Supported _ (Cancelling _ _) ->
            "Couldn't download the model"

        Supported _ (Generating _ _ _) ->
            "Couldn't generate the picture"

        Supported _ Clearing ->
            "Couldn't remove the downloaded models"

        Unsupported Emptying ->
            "Couldn't remove the downloaded models"

        Supported _ (Releasing _) ->
            "Couldn't release the model"

        _ ->
            "Couldn't start the studio"


failedRetry : Device -> Notice.Retry Msg
failedRetry device =
    case device of
        Supported _ (Loading chosen _ Nothing) ->
            Notice.Retry (DownloadClicked chosen)

        Supported _ (Loading _ _ (Just _)) ->
            Notice.Retry GenerateClicked

        Supported _ (Generating _ _ (Denoising _)) ->
            Notice.Retry GenerateClicked

        _ ->
            Notice.NoRetry


isCancellable : Device -> Bool
isCancellable device =
    case device of
        Supported _ (Cancelling _ _) ->
            True

        Supported _ (Generating _ _ Stopping) ->
            True

        _ ->
            False


rest : Device -> Maybe Device
rest device =
    case device of
        Unchecked ->
            Just (Unsupported Unreachable)

        Checking ->
            Just (Unsupported Unreachable)

        Unsupported Kept ->
            Just (Unsupported Unreachable)

        Unsupported Emptying ->
            Just (Unsupported Kept)

        Unsupported Unreachable ->
            Nothing

        Supported can running ->
            Maybe.map (Supported can) (restEngine running)


restEngine : Engine -> Maybe Engine
restEngine engine =
    case engine of
        Empty ->
            Nothing

        Clearing ->
            Just Empty

        Loading _ _ _ ->
            Just Empty

        Cancelling _ _ ->
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


plan : String -> Model -> Maybe ( Job, Random.Seed )
plan text { settings, random } =
    if List.isEmpty settings.unreadable then
        planReadable text settings random

    else
        Nothing


planReadable : String -> Settings -> Random.Seed -> Maybe ( Job, Random.Seed )
planReadable text settings random =
    Maybe.map3
        (\prompt steps ( seed, next ) ->
            ( { prompt = prompt
              , width = settings.width
              , height = settings.height
              , steps = steps
              , seed = seed
              }
            , next
            )
        )
        (Prompt.parse text)
        (Steps.fromString settings.steps)
        (draw settings.seeding random)


checks : Model -> List Validation
checks { settings, random, checked } =
    let
        check field problem live valid =
            { field = field, problem = problem, failing = (live || checked) && not valid }
    in
    [ check "prompt"
        "Write a prompt to generate a picture."
        False
        (case settings.prompt of
            Editing text ->
                Prompt.parse text /= Nothing

            Styling ->
                True
        )
    , check "steps"
        ("Steps must be a whole number from " ++ String.fromInt Steps.fewest ++ " to " ++ String.fromInt Steps.most ++ ".")
        True
        (Steps.fromString settings.steps /= Nothing && not (List.member StepsField settings.unreadable))
    , check "seed"
        ("Seed must be a whole number from 0 to " ++ String.fromInt Seed.largest ++ ".")
        True
        (draw settings.seeding random /= Nothing && not (List.member SeedField settings.unreadable))
    ]


draw : Seeding -> Random.Seed -> Maybe ( Seed, Random.Seed )
draw seeding random =
    case seeding of
        RandomSeed ->
            Just (Random.step Seed.generator random)

        FixedSeed text ->
            Seed.fromString text
                |> Maybe.map (\seed -> ( seed, random ))


edit : (Settings -> Settings) -> Model -> Model
edit change model =
    { model | settings = change model.settings }


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
