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
    , Picture
    , Seeding(..)
    , Settings
    , Stage(..)
    , Weights(..)
    , components
    , download
    , enter
    , frameBudget
    , guidance
    , init
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
import Random



-- MODEL


type alias Model =
    { engine : Engine
    , settings : Settings
    , message : Maybe Message
    , pictures : List Picture
    , random : Random.Seed
    }


type Engine
    = Unchecked
    | Checking
    | Unsupported
    | Empty
    | Clearing
    | Loading Weights (List Download)
    | Ready Weights
    | Generating Weights Job Stage
    | Releasing


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
    , keep : Bool
    , fresh : Bool
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


init : Int -> Model
init entropy =
    { engine = Unchecked
    , settings =
        { weights = Ternary
        , keep = True
        , fresh = False
        , prompt = ""
        , width = Dimension.default
        , height = Dimension.default
        , steps = Steps.default
        , seeding = RandomSeed
        }
    , message = Nothing
    , pictures = []
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


components : List Component
components =
    [ TextEncoder, Transformer, Vae ]


download : Component -> List Download -> Maybe Download
download component downloads =
    List.filter (\known -> known.component == component) downloads
        |> List.head


guidance : Float
guidance =
    1


frameBudget : Int
frameBudget =
    8



-- UPDATE


type Msg
    = WeightsChosen String
    | KeepToggled Bool
    | FreshToggled Bool
    | LoadClicked
    | CancelClicked
    | ReleaseClicked
    | ClearClicked
    | PromptChanged String
    | WidthChanged Float
    | HeightChanged Float
    | StepsChanged Float
    | RandomSeedToggled Bool
    | SeedChanged String
    | SeedReused Seed
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
    | Load { weights : Weights, keep : Bool, fresh : Bool }
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
        WeightsChosen id ->
            ( edit (\settings -> { settings | weights = Maybe.withDefault settings.weights (weightsFromId id) }) model, Nothing )

        KeepToggled keep ->
            ( edit (\settings -> { settings | keep = keep }) model, Nothing )

        FreshToggled fresh ->
            ( edit (\settings -> { settings | fresh = fresh }) model, Nothing )

        PromptChanged prompt ->
            ( edit (\settings -> { settings | prompt = prompt }) model, Nothing )

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

        SeedReused seed ->
            ( edit (\settings -> { settings | seeding = FixedSeed (Seed.toString seed) }) model, Nothing )

        LoadClicked ->
            case model.engine of
                Empty ->
                    ( { model | engine = Loading model.settings.weights [], message = Nothing }
                    , Just
                        (Load
                            { weights = model.settings.weights
                            , keep = model.settings.keep
                            , fresh = model.settings.fresh
                            }
                        )
                    )

                _ ->
                    ( model, Nothing )

        CancelClicked ->
            case model.engine of
                Loading _ _ ->
                    ( model, Just Cancel )

                Generating loaded job (Denoising _) ->
                    ( { model | engine = Generating loaded job Stopping }, Just Cancel )

                _ ->
                    ( model, Nothing )

        ReleaseClicked ->
            case model.engine of
                Ready _ ->
                    ( { model | engine = Releasing, message = Nothing }, Just Release )

                _ ->
                    ( model, Nothing )

        ClearClicked ->
            case model.engine of
                Empty ->
                    ( { model | engine = Clearing, message = Nothing }, Just ClearCache )

                _ ->
                    ( model, Nothing )

        GenerateClicked ->
            case ( model.engine, plan model ) of
                ( Ready loaded, Ok ( job, random ) ) ->
                    ( { model | engine = Generating loaded job (Denoising 0), message = Nothing, random = random }
                    , Just (Generate job)
                    )

                ( Ready _, Err problem ) ->
                    ( { model | message = Just (Problem problem) }, Nothing )

                _ ->
                    ( model, Nothing )

        Happened event ->
            ( happened event model, Nothing )


happened : Event -> Model -> Model
happened event model =
    case ( event, model.engine ) of
        ( SupportChecked True, Checking ) ->
            { model | engine = Empty }

        ( SupportChecked False, Checking ) ->
            { model | engine = Unsupported }

        ( Downloaded progress, Loading chosen downloads ) ->
            { model
                | engine =
                    Loading chosen
                        (progress :: List.filter (\known -> known.component /= progress.component) downloads)
            }

        ( ModelLoaded, Loading chosen _ ) ->
            edit (\settings -> { settings | fresh = False }) { model | engine = Ready chosen }

        ( StepFinished index, Generating loaded job (Denoising _) ) ->
            { model | engine = Generating loaded job (Denoising (index + 1)) }

        ( PictureMade url, Generating loaded job _ ) ->
            { model | engine = Ready loaded, pictures = { url = url, job = job } :: model.pictures }

        ( ModelReleased, Releasing ) ->
            { model | engine = Empty }

        ( CacheCleared, Clearing ) ->
            { model | engine = Empty, message = Just (Info "The downloaded models are gone from this browser.") }

        ( Failed failure, _ ) ->
            case rest model.engine of
                Just engine ->
                    { model
                        | engine = engine
                        , message =
                            if failure.name == "AbortError" then
                                Nothing

                            else
                                Just (Problem (explain failure))
                    }

                Nothing ->
                    model

        _ ->
            model


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

        Loading _ _ ->
            Just Empty

        Ready _ ->
            Nothing

        Generating loaded _ _ ->
            Just (Ready loaded)

        Releasing ->
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
