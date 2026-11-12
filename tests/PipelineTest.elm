module PipelineTest exposing (suite)

import Data.Dimension as Dimension
import Data.Seed as Seed
import Data.Steps as Steps
import Expect
import Pipeline exposing (Command(..), Engine(..), Event(..), Message(..), Msg(..), Seeding(..), Stage(..), Weights(..))
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
            , test "download again only once" <|
                \() ->
                    run [ FreshToggled True, LoadClicked, Happened ModelLoaded ] supported
                        |> .settings
                        |> .fresh
                        |> Expect.equal False
            ]
        , describe "update"
            [ test "checks the browser on the first visit only" <|
                \() ->
                    Pipeline.enter (Pipeline.init 1)
                        |> Tuple.mapFirst Pipeline.enter
                        |> (\( ( _, second ), first ) -> ( first, second ))
                        |> Expect.equal ( Just Check, Nothing )
            , test "loads nothing while a model is in memory" <|
                \() ->
                    Pipeline.update LoadClicked ready
                        |> Tuple.second
                        |> Expect.equal Nothing
            , test "refuses a seed that is not a whole number in range" <|
                \() ->
                    List.map
                        (\seed ->
                            run [ PromptChanged "a bonsai", SeedChanged seed, GenerateClicked ] ready
                                |> .engine
                        )
                        [ "", "-1", "1.5", "4294967296", "seven" ]
                        |> Expect.equal (List.repeat 5 (Ready Ternary))
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
                        |> (\model -> ( model.engine, model.message ))
                        |> Expect.equal ( Ready Ternary, Just (Problem "Out of memory") )
            , test "names a failure that says nothing" <|
                \() ->
                    run [ LoadClicked, Happened (Failed { name = "OperationError", message = "" }) ] supported
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
                        |> (\model -> ( model.engine, List.map .url model.pictures ))
                        |> Expect.equal ( Ready Ternary, [ "blob:1" ] )
            , test "ignores what arrives for work it did not start" <|
                \() ->
                    run [ Happened ModelLoaded, Happened (PictureMade "blob:1"), Happened (StepFinished 2) ] supported
                        |> (\model -> ( model.engine, model.pictures ))
                        |> Expect.equal ( Empty, [] )
            , test "counts the steps that are done" <|
                \() ->
                    run [ PromptChanged "a bonsai", GenerateClicked, Happened (StepFinished 0), Happened (StepFinished 1) ] ready
                        |> .engine
                        |> stage
                        |> Expect.equal (Just (Denoising 2))
            ]
        ]


supported : Pipeline.Model
supported =
    Pipeline.enter (Pipeline.init 1)
        |> Tuple.first
        |> run [ Happened (SupportChecked True) ]


ready : Pipeline.Model
ready =
    run [ LoadClicked, Happened ModelLoaded, RandomSeedToggled False ] supported


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


stage : Engine -> Maybe Stage
stage engine =
    case engine of
        Generating _ _ found ->
            Just found

        _ ->
            Nothing
