module PipelineTest exposing (suite)

import Data.Dimension as Dimension
import Data.Prompt as Prompt
import Data.Seed as Seed
import Data.Steps as Steps
import Expect
import Pipeline exposing (Command(..), Component(..), Device(..), Draft(..), Engine(..), Event(..), Message(..), Msg(..), Prompting(..), Seeding(..), Stage(..), Storage(..), Weights(..))
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
            [ test "start square, at the first preset's size" <|
                \() ->
                    Pipeline.init 1 True []
                        |> .settings
                        |> (\settings -> List.filter (\preset -> preset.width == settings.width && preset.height == settings.height) Pipeline.presets)
                        |> List.map .ratio
                        |> Expect.equal [ "1:1" ]
            , test "make a picture with the steps typed" <|
                \() ->
                    Pipeline.update GenerateClicked (run [ PromptChanged "a bonsai", StepsChanged " 20 " ] ready)
                        |> Tuple.second
                        |> Maybe.andThen stepsOf
                        |> Expect.equal (Just 20)
            , test "refuse steps the library doesn't take" <|
                \() ->
                    List.map
                        (\steps ->
                            Pipeline.update GenerateClicked (run [ PromptChanged "a bonsai", StepsChanged steps ] ready)
                                |> Tuple.mapFirst .message
                        )
                        [ "", "0", "51", "4.5", "four" ]
                        |> Expect.equal (List.repeat 5 ( Just (Problem "Steps are a whole number from 1 to 50."), Nothing ))
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
            , test "are removed on a browser without WebGPU" <|
                \() ->
                    Pipeline.init 1 False [ Binary ]
                        |> run [ ClearClicked, Happened CacheCleared ]
                        |> (\model -> ( model.device, model.downloaded, model.message ))
                        |> Expect.equal ( Unsupported Kept, [], Just (Info "Downloaded models removed.") )
            , test "are not removed through a worker that failed" <|
                \() ->
                    Pipeline.init 1 True [ Binary ]
                        |> run [ Happened (Failed { name = "Error", message = "Worker unable to start" }) ]
                        |> Pipeline.update ClearClicked
                        |> Tuple.mapFirst .device
                        |> Expect.equal ( Unsupported Unreachable, Nothing )
            , test "are not removed through a worker that failed on a browser without WebGPU" <|
                \() ->
                    Pipeline.init 1 False [ Binary ]
                        |> run [ Happened (Failed { name = "Error", message = "Worker unable to start" }) ]
                        |> Pipeline.update ClearClicked
                        |> Tuple.mapFirst (\model -> ( model.device, model.message ))
                        |> Expect.equal ( ( Unsupported Unreachable, Just (Problem "Worker unable to start") ), Nothing )
            , test "stay when removing them fails without WebGPU" <|
                \() ->
                    Pipeline.init 1 False [ Binary ]
                        |> run [ ClearClicked, Happened (Failed { name = "Error", message = "Blocked" }) ]
                        |> (\model -> ( model.device, model.downloaded ))
                        |> Expect.equal ( Unsupported Kept, [ Binary ] )
            , test "say they are cancelling until the download stops, quietly" <|
                \() ->
                    run [ DownloadClicked Binary, CancelClicked ] supported
                        |> (\cancelling -> ( cancelling.device, run [ Happened (Failed { name = "AbortError", message = "" }) ] cancelling |> .message ))
                        |> Expect.equal ( Supported CanPrompt (Cancelling Binary []), Nothing )
            , test "keep a model that finished loading as the download was cancelled, and make no picture with it" <|
                \() ->
                    Pipeline.update (Happened ModelLoaded) (run [ PromptChanged "a bonsai", GenerateClicked, CancelClicked ] supported)
                        |> Tuple.mapFirst (\model -> ( model.device, model.downloaded ))
                        |> Expect.equal ( ( Supported CanPrompt (Ready Ternary), [ Ternary ] ), Nothing )
            , test "say why a download stopped that nobody cancelled" <|
                \() ->
                    run [ DownloadClicked Binary, Happened (Failed { name = "AbortError", message = "The device was lost." }) ] supported
                        |> (\model -> ( model.device, model.message ))
                        |> Expect.equal ( Supported CanPrompt Empty, Just (Problem "The device was lost.") )
            , test "are not remembered when the download stops halfway" <|
                \() ->
                    run [ DownloadClicked Binary, CancelClicked, Happened (Failed { name = "AbortError", message = "" }) ] supported
                        |> (\model -> ( model.device, model.downloaded ))
                        |> Expect.equal ( Supported CanPrompt Empty, [] )
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
                                    Supported _ (Loading _ parts _) ->
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
            [ test "gives up on the browser when the worker failed before the check" <|
                \() ->
                    Pipeline.init 1 True []
                        |> Pipeline.update (Happened (Failed { name = "Error", message = "Worker unable to start" }))
                        |> Tuple.first
                        |> Pipeline.enter
                        |> Tuple.mapFirst (\model -> ( model.device, model.message ))
                        |> Expect.equal ( ( Unsupported Unreachable, Just (Problem "Worker unable to start") ), Nothing )
            , test "never checks a browser without the WebGPU API" <|
                \() ->
                    Pipeline.enter (Pipeline.init 1 False [])
                        |> Tuple.mapFirst .device
                        |> Expect.equal ( Unsupported Kept, Nothing )
            , test "gives up on the browser when the check fails" <|
                \() ->
                    Pipeline.enter (Pipeline.init 1 True [])
                        |> Tuple.first
                        |> run [ Happened (Failed { name = "Error", message = "Worker unable to start" }) ]
                        |> .device
                        |> Expect.equal (Unsupported Unreachable)
            , test "starts nothing while it checks the browser" <|
                \() ->
                    Pipeline.enter (Pipeline.init 1 True [])
                        |> Tuple.first
                        |> run [ PromptChanged "a bonsai" ]
                        |> (\checking -> [ Pipeline.update GenerateClicked checking, Pipeline.update (DownloadClicked Ternary) checking ])
                        |> List.map Tuple.second
                        |> Expect.equal [ Nothing, Nothing ]
            , test "checks the browser on the first visit only" <|
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
                        |> Expect.equal (List.repeat 5 (Supported CanPrompt (Ready Ternary)))
            , test "takes the largest seed" <|
                \() ->
                    Pipeline.update GenerateClicked (run [ PromptChanged "a bonsai", SeedChanged "4294967295" ] ready)
                        |> Tuple.second
                        |> Maybe.andThen seedOf
                        |> Expect.equal (Just Seed.largest)
            , test "makes no picture while AI Style writes the prompt" <|
                \() ->
                    Pipeline.update GenerateClicked (run [ PromptChanged "a bonsai", StylingStarted ] ready)
                        |> Tuple.mapFirst (\model -> ( model.settings.prompt, model.message ))
                        |> Expect.equal ( ( Styling, Nothing ), Nothing )
            , test "keeps what is typed out of the prompt AI Style writes" <|
                \() ->
                    run [ PromptChanged "a bonsai", StylingStarted, PromptChanged "a cactus" ] ready
                        |> .settings
                        |> .prompt
                        |> Expect.equal Styling
            , test "makes the picture AI Style wrote" <|
                \() ->
                    Pipeline.update GenerateClicked (run [ PromptChanged "a bonsai", StylingStarted, StylingEnded "Subject: a bonsai" ] ready)
                        |> Tuple.second
                        |> Maybe.andThen promptOf
                        |> Expect.equal (Just "Subject: a bonsai")
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
                        |> Expect.equal ( Supported CanPrompt (Ready Ternary), Just (Problem "Out of memory") )
            , test "says so when a picture is aborted and nobody asked" <|
                \() ->
                    run
                        [ PromptChanged "a bonsai"
                        , GenerateClicked
                        , Happened (Failed { name = "AbortError", message = "Device was lost before MapAsync completed." })
                        ]
                        ready
                        |> (\model -> ( model.device, model.message ))
                        |> Expect.equal ( Supported CanPrompt (Ready Ternary), Just (Problem "Device was lost before MapAsync completed.") )
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
                        |> Expect.equal ( Supported CanPrompt (Ready Ternary), Nothing )
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
                        |> Expect.equal ( Supported CanPrompt (Ready Ternary), [ "blob:1" ] )
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
                        |> Expect.equal ( Supported CanPrompt Empty, [] )
            , test "counts the steps that are done" <|
                \() ->
                    run [ PromptChanged "a bonsai", GenerateClicked, Happened (StepFinished 0), Happened (StepFinished 1) ] ready
                        |> .device
                        |> stage
                        |> Expect.equal (Just (Denoising 2))
            , test "can write prompts only on an adapter with 16-bit shaders" <|
                \() ->
                    [ Just [ "shader-f16", "subgroups" ], Just [ "subgroups" ], Just [], Nothing ]
                        |> List.map
                            (\features ->
                                Pipeline.enter (Pipeline.init 1 True [])
                                    |> Tuple.first
                                    |> run [ Happened (SupportChecked features) ]
                                    |> .device
                            )
                        |> Expect.equal
                            [ Supported CanPrompt Empty
                            , Supported NoShaderF16 Empty
                            , Supported NoShaderF16 Empty
                            , Unsupported Kept
                            ]
            , test "makes pictures without 16-bit shaders" <|
                \() ->
                    Pipeline.enter (Pipeline.init 1 True [])
                        |> Tuple.first
                        |> run [ Happened (SupportChecked (Just [])) ]
                        |> Pipeline.update (DownloadClicked Ternary)
                        |> Tuple.second
                        |> Expect.equal (Just (Load Ternary))
            , test "keeps what the prompt model can do while the image model changes" <|
                \() ->
                    Pipeline.enter (Pipeline.init 1 True [])
                        |> Tuple.first
                        |> run [ Happened (SupportChecked (Just [])), DownloadClicked Ternary, Happened ModelLoaded, ReleaseClicked, Happened ModelReleased ]
                        |> .device
                        |> Expect.equal (Supported NoShaderF16 Empty)
            , test "says the prompt model runs only with 16-bit shaders" <|
                \() ->
                    List.map Pipeline.canPrompt
                        [ Unchecked, Checking, Unsupported Kept, Supported CanPrompt Empty, Supported NoShaderF16 (Ready Ternary) ]
                        |> Expect.equal [ False, False, False, True, False ]
            ]
        ]


supported : Pipeline.Model
supported =
    Pipeline.enter (Pipeline.init 1 True [])
        |> Tuple.first
        |> run [ Happened (SupportChecked (Just [ "shader-f16" ])) ]


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


stepsOf : Command -> Maybe Int
stepsOf command =
    case command of
        Generate job ->
            Just (Steps.toInt job.steps)

        _ ->
            Nothing


promptOf : Command -> Maybe String
promptOf command =
    case command of
        Generate job ->
            Just (Prompt.toString job.prompt)

        _ ->
            Nothing


stage : Device -> Maybe Stage
stage device =
    case device of
        Supported _ (Generating _ _ found) ->
            Just found

        _ ->
            Nothing
