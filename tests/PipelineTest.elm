module PipelineTest exposing (suite)

import Data.Dimension as Dimension
import Data.Seed as Seed
import Data.Steps as Steps
import Expect
import Pipeline exposing (Command(..), Component(..), Device(..), Engine(..), Event(..), Message(..), Msg(..), Seeding(..), Stage(..), Weights(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Pipeline"
        [ describe "weights"
            [ test "are found again by the id the library loads them by" <|
                \() ->
                    List.map (Pipeline.weightsId >> Pipeline.weightsFromId) Pipeline.weights
                        |> Expect.equal (List.map Just Pipeline.weights)
            , test "are only the ones the browser can run" <|
                \() ->
                    List.map Pipeline.weightsId Pipeline.weights
                        |> List.filter (not << String.contains "-mlx-")
                        |> Expect.equal []
            ]
        , describe "settings"
            [ test "keep a width that is off the grid out" <|
                \() ->
                    run [ WidthChanged 700 ] ready
                        |> .settings
                        |> .width
                        |> Dimension.toInt
                        |> Expect.equal 512
            , test "take a width and a height apart" <|
                \() ->
                    run [ WidthChanged 1024, HeightChanged 256 ] ready
                        |> .settings
                        |> (\settings -> ( Dimension.toInt settings.width, Dimension.toInt settings.height ))
                        |> Expect.equal ( 1024, 256 )
            , test "keep more steps than the library takes out" <|
                \() ->
                    run [ StepsChanged 20, StepsChanged 51 ] ready
                        |> .settings
                        |> .steps
                        |> Steps.toInt
                        |> Expect.equal 20
            , test "start a fixed seed from the last picture's" <|
                \() ->
                    run
                        [ PromptChanged "a bonsai"
                        , SeedChanged "31"
                        , GenerateClicked
                        , Happened (PictureMade "blob:1")
                        , RandomSeedToggled True
                        , RandomSeedToggled False
                        ]
                        ready
                        |> .settings
                        |> .seeding
                        |> Expect.equal (FixedSeed "31")
            , test "take a preset's width and height" <|
                \() ->
                    List.filter (\preset -> preset.name == "Story") Pipeline.presets
                        |> List.map (\preset -> run [ PresetChosen preset ] ready)
                        |> List.map (\model -> ( Dimension.toInt model.settings.width, Dimension.toInt model.settings.height ))
                        |> Expect.equal [ ( 576, 1024 ) ]
            ]
        , describe "presets"
            [ test "are all on the grid the model takes" <|
                \() ->
                    List.map .name Pipeline.presets
                        |> Expect.equal [ "Square", "Portrait", "Story", "Widescreen", "Pin", "Link card", "Banner" ]
            , test "have the shape they name" <|
                \() ->
                    List.map
                        (\preset ->
                            case List.filterMap String.toInt (String.split ":" preset.ratio) of
                                [ across, down ] ->
                                    Dimension.toInt preset.width * down == Dimension.toInt preset.height * across

                                _ ->
                                    False
                        )
                        Pipeline.presets
                        |> Expect.equal (List.repeat 7 True)
            ]
        , describe "models"
            [ test "are remembered once they have loaded" <|
                \() ->
                    run [ DownloadClicked Binary, Happened ModelLoaded ] supported
                        |> .downloaded
                        |> Expect.equal [ Binary ]
            , test "are not remembered when the download stops halfway" <|
                \() ->
                    run [ DownloadClicked Binary, CancelClicked, Happened (Failed { name = "AbortError", message = "" }) ] supported
                        |> (\model -> ( model.device, model.downloaded ))
                        |> Expect.equal ( Supported Empty, [] )
            , test "load one part at a time, the last one heard of" <|
                \() ->
                    run
                        [ DownloadClicked Ternary
                        , Happened (Downloaded { component = TextEncoder, loaded = 5, total = 5, cached = False })
                        , Happened (Downloaded { component = Transformer, loaded = 1, total = 9, cached = False })
                        , Happened (Downloaded { component = Transformer, loaded = 2, total = 9, cached = False })
                        ]
                        supported
                        |> .device
                        |> (\device ->
                                case device of
                                    Supported (Loading _ parts _) ->
                                        Maybe.map (\part -> ( part.component, part.loaded )) (Pipeline.current parts)

                                    _ ->
                                        Nothing
                           )
                        |> Expect.equal (Just ( Transformer, 2 ))
            , test "are forgotten when the downloads are removed" <|
                \() ->
                    run [ DownloadClicked Binary, Happened ModelLoaded, ReleaseClicked, Happened ModelReleased, ClearClicked, Happened CacheCleared ] supported
                        |> .downloaded
                        |> Expect.equal []
            , test "come back from what the browser stored" <|
                \() ->
                    Pipeline.remembered (Pipeline.remember [ Binary, Ternary ])
                        |> Expect.equal [ Binary, Ternary ]
            , test "leave out a stored model that is not offered any more" <|
                \() ->
                    Pipeline.remembered """["prism-ml/bonsai-image-binary-4B-mlx-1bit","gone"]"""
                        |> Expect.equal [ Binary ]
            , test "are none when what the browser stored is no list" <|
                \() ->
                    Pipeline.remembered "nonsense"
                        |> Expect.equal []
            ]
        , describe "update"
            [ test "checks the browser on the first visit only" <|
                \() ->
                    Pipeline.enter (Pipeline.init 1 True [])
                        |> Tuple.mapFirst Pipeline.enter
                        |> (\( ( _, second ), first ) -> ( first, second ))
                        |> Expect.equal ( Just Check, Nothing )
            , test "loads nothing while that model is in memory" <|
                \() ->
                    Pipeline.update (DownloadClicked Ternary) ready
                        |> Tuple.second
                        |> Expect.equal Nothing
            , test "loads the model a picture needs, then makes the picture" <|
                \() ->
                    Pipeline.update GenerateClicked (run [ PromptChanged "a bonsai", SeedChanged "7" ] supported)
                        |> Tuple.mapFirst (Pipeline.update (Happened ModelLoaded) >> Tuple.second >> Maybe.andThen seedOf)
                        |> Expect.equal ( Just 7, Just (Load Ternary) )
            , test "makes room for another model before it loads it" <|
                \() ->
                    Pipeline.update GenerateClicked
                        (run [ PromptChanged "a bonsai", WeightsChosen Binary ] ready)
                        |> Tuple.mapFirst (Pipeline.update (Happened ModelReleased) >> Tuple.second)
                        |> Expect.equal ( Just (Load Binary), Just Release )
            , test "asks for a prompt before it loads anything" <|
                \() ->
                    Pipeline.update GenerateClicked supported
                        |> Tuple.mapFirst .message
                        |> Expect.equal ( Just (Problem "Describe the picture first."), Nothing )
            , test "refuses a seed that is not a whole number in range" <|
                \() ->
                    List.map
                        (\seed ->
                            run [ PromptChanged "a bonsai", SeedChanged seed, GenerateClicked ] ready
                                |> .device
                        )
                        [ "", "-1", "1.5", "4294967296", "seven" ]
                        |> Expect.equal (List.repeat 5 (Supported (Ready Ternary)))
            , test "takes the largest seed" <|
                \() ->
                    Pipeline.update GenerateClicked (run [ PromptChanged "a bonsai", SeedChanged "4294967295" ] ready)
                        |> Tuple.second
                        |> Maybe.andThen seedOf
                        |> Expect.equal (Just Seed.largest)
            , test "makes one picture at a time" <|
                \() ->
                    Pipeline.update GenerateClicked (run [ PromptChanged "a bonsai", GenerateClicked ] ready)
                        |> Tuple.second
                        |> Expect.equal Nothing
            , test "is ready again after a picture fails, and says why" <|
                \() ->
                    run
                        [ PromptChanged "a bonsai"
                        , GenerateClicked
                        , Happened (Failed { name = "Error", message = "Out of memory" })
                        ]
                        ready
                        |> (\model -> ( model.device, model.message ))
                        |> Expect.equal ( Supported (Ready Ternary), Just (Problem "Out of memory") )
            , test "says so when a picture is aborted and nobody asked" <|
                \() ->
                    run
                        [ PromptChanged "a bonsai"
                        , GenerateClicked
                        , Happened (Failed { name = "AbortError", message = "Device was lost before MapAsync completed." })
                        ]
                        ready
                        |> (\model -> ( model.device, model.message ))
                        |> Expect.equal ( Supported (Ready Ternary), Just (Problem "Device was lost before MapAsync completed.") )
            , test "says nothing when a picture is aborted on request" <|
                \() ->
                    run
                        [ PromptChanged "a bonsai"
                        , GenerateClicked
                        , CancelClicked
                        , Happened (Failed { name = "AbortError", message = "The operation was aborted." })
                        ]
                        ready
                        |> (\model -> ( model.device, model.message ))
                        |> Expect.equal ( Supported (Ready Ternary), Nothing )
            , test "names a failure that says nothing" <|
                \() ->
                    run [ DownloadClicked Ternary, Happened (Failed { name = "OperationError", message = "" }) ] supported
                        |> .message
                        |> Expect.equal (Just (Problem "Something went wrong: OperationError."))
            , test "keeps the picture that arrives after a cancel" <|
                \() ->
                    run
                        [ PromptChanged "a bonsai"
                        , GenerateClicked
                        , CancelClicked
                        , Happened (PictureMade "blob:1")
                        ]
                        ready
                        |> (\model -> ( model.device, List.map .url model.pictures ))
                        |> Expect.equal ( Supported (Ready Ternary), [ "blob:1" ] )
            , test "opens a picture with its details hidden, and closes it" <|
                \() ->
                    [ [ Opened "blob:1" ]
                    , [ Opened "blob:1", DetailsToggled ]
                    , [ Opened "blob:1", DetailsToggled, Closed, Opened "blob:2" ]
                    , [ Opened "blob:1", Closed ]
                    , Opened "blob:1" :: List.filterMap (Seed.fromInt >> Maybe.map SeedReused) [ 7 ]
                    ]
                        |> List.map (\msgs -> run msgs ready |> (\model -> ( model.viewing, model.zoomed, model.detailed )))
                        |> Expect.equal
                            [ ( Just "blob:1", True, False )
                            , ( Just "blob:1", True, True )
                            , ( Just "blob:2", True, False )
                            , ( Just "blob:1", False, False )
                            , ( Just "blob:1", False, False )
                            ]
            , test "steps to the picture beside the open one, and stops at either end" <|
                \() ->
                    let
                        two =
                            run [ PromptChanged "a bonsai", GenerateClicked, Happened (PictureMade "blob:1"), GenerateClicked, Happened (PictureMade "blob:2") ] ready
                    in
                    [ [ Opened "blob:2", Stepped 1 ]
                    , [ Opened "blob:1", Stepped -1 ]
                    , [ Opened "blob:1", Stepped 1 ]
                    , [ Opened "blob:2", Stepped -1 ]
                    , [ Opened "blob:2", Closed, Stepped 1 ]
                    ]
                        |> List.map (\msgs -> (run msgs two).viewing)
                        |> Expect.equal [ Just "blob:1", Just "blob:2", Just "blob:1", Just "blob:2", Just "blob:2" ]
            , test "ignores what arrives for work it did not start" <|
                \() ->
                    run [ Happened ModelLoaded, Happened (PictureMade "blob:1"), Happened (StepFinished 2) ] supported
                        |> (\model -> ( model.device, model.pictures ))
                        |> Expect.equal ( Supported Empty, [] )
            , test "counts the steps that are done" <|
                \() ->
                    run [ PromptChanged "a bonsai", GenerateClicked, Happened (StepFinished 0), Happened (StepFinished 1) ] ready
                        |> .device
                        |> stage
                        |> Expect.equal (Just (Denoising 2))
            ]
        ]


supported : Pipeline.Model
supported =
    Pipeline.enter (Pipeline.init 1 True [])
        |> Tuple.first
        |> run [ Happened (SupportChecked True) ]


ready : Pipeline.Model
ready =
    run [ DownloadClicked Ternary, Happened ModelLoaded, RandomSeedToggled False ] supported


run : List Msg -> Pipeline.Model -> Pipeline.Model
run msgs model =
    List.foldl (\msg -> Pipeline.update msg >> Tuple.first) model msgs


seedOf : Command -> Maybe Int
seedOf command =
    case command of
        Generate job ->
            Just (Seed.toInt job.seed)

        _ ->
            Nothing


stage : Device -> Maybe Stage
stage device =
    case device of
        Supported (Generating _ _ found) ->
            Just found

        _ ->
            Nothing
