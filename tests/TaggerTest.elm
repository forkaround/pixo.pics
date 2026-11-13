module TaggerTest exposing (suite)

import Data.Tag as Tag
import Expect
import Stylist exposing (Role(..))
import Tagger exposing (Command(..), Event(..), Msg(..), State(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Tagger"
        [ describe "update"
            [ test "loads when asked while asleep" <|
                \() ->
                    step [ Asked [ job 1 ] ]
                        |> Expect.equal ( Loading, Just Load, Nothing )
            , test "asked twice for the same picture, it queues it once and gives no command" <|
                \() ->
                    let
                        ( model, _, _ ) =
                            run [ Asked [ job 1 ] ] Tagger.init

                        ( again, command, _ ) =
                            Tagger.update (Asked [ job 1 ]) model
                    in
                    Expect.equal ( Nothing, Tagger.pending again ) ( command, 1 )
            , test "tags the first picture once it is loaded" <|
                \() ->
                    step [ Asked [ job 1 ], Happened Loaded ]
                        |> Expect.equal ( Tagging (job 1), Just (Tag (Tagger.request (job 1))), Nothing )
            , test "reports what it found and releases when the queue is empty" <|
                \() ->
                    let
                        ( state, command, found ) =
                            step [ Asked [ job 1 ], Happened Loaded, Happened (Finished "<think></think>fox, snow, Winter.") ]
                    in
                    Expect.equal ( Releasing, Just Release, Just ( 1, [ "fox", "snow", "winter" ] ) )
                        ( state, command, Maybe.map (\f -> ( f.id, List.map Tag.toString f.tags )) found )
            , test "goes on to the next picture after one is finished" <|
                \() ->
                    step [ Asked [ job 1, job 2 ], Happened Loaded, Happened (Finished "fox, snow") ]
                        |> (\( state, command, _ ) -> ( state, command ))
                        |> Expect.equal ( Tagging (job 2), Just (Tag (Tagger.request (job 2))) )
            , test "reports nothing for an empty answer" <|
                \() ->
                    step [ Asked [ job 1 ], Happened Loaded, Happened (Finished "") ]
                        |> (\( _, _, found ) -> Maybe.map .id found)
                        |> Expect.equal Nothing
            , test "gives up when loading fails, and does not retry the same picture" <|
                \() ->
                    let
                        ( model, _, _ ) =
                            run [ Asked [ job 1 ], Happened (Failed failure) ] Tagger.init

                        ( _, command, _ ) =
                            Tagger.update (Asked [ job 1 ]) model
                    in
                    Expect.equal ( Asleep, [], Nothing ) ( model.state, model.queue, command )
            , test "skips a picture it fails to tag" <|
                \() ->
                    step [ Asked [ job 1, job 2 ], Happened Loaded, Happened (Failed failure) ]
                        |> (\( state, command, _ ) -> ( state, command ))
                        |> Expect.equal ( Tagging (job 2), Just (Tag (Tagger.request (job 2))) )
            , test "loads again after releasing when a picture was queued meanwhile" <|
                \() ->
                    step [ Asked [ job 1 ], Happened Loaded, Happened (Finished "fox, snow"), Asked [ job 2 ], Happened Released ]
                        |> (\( state, command, _ ) -> ( state, command ))
                        |> Expect.equal ( Loading, Just Load )
            , test "sleeps after releasing with nothing queued" <|
                \() ->
                    step [ Asked [ job 1 ], Happened Loaded, Happened (Finished "fox, snow"), Happened Released ]
                        |> (\( state, command, _ ) -> ( state, command ))
                        |> Expect.equal ( Asleep, Nothing )
            , test "ignores an event it is not waiting for" <|
                \() ->
                    step [ Happened Loaded ]
                        |> Expect.equal ( Asleep, Nothing, Nothing )
            , test "counts the queue plus the picture being tagged" <|
                \() ->
                    run [ Asked [ job 1, job 2, job 3 ], Happened Loaded ] Tagger.init
                        |> (\( model, _, _ ) -> Tagger.pending model)
                        |> Expect.equal 3
            , test "is asleep only when nothing is going on" <|
                \() ->
                    Expect.equal ( True, False )
                        ( Tagger.isAsleep Tagger.init
                        , Tagger.isAsleep (run [ Asked [ job 1 ] ] Tagger.init |> (\( model, _, _ ) -> model))
                        )
            ]
        , describe "conversation"
            [ test "starts with the instructions and ends with the prompt" <|
                \() ->
                    let
                        turns =
                            Tagger.conversation (job 1)
                    in
                    Expect.equal ( Just System, Just { role = User, content = "a fox 1" } )
                        ( Maybe.map .role (List.head turns), List.reverse turns |> List.head )
            ]
        , describe "tags"
            [ test "keeps at most 8" <|
                \() ->
                    Tagger.tags "aa, bb, cc, dd, ee, ff, gg, hh, ii, jj"
                        |> List.length
                        |> Expect.equal 8
            , test "drops duplicates" <|
                \() ->
                    Tagger.tags "fox, Fox, snow"
                        |> List.map Tag.toString
                        |> Expect.equal [ "fox", "snow" ]
            , test "reads one tag per line too" <|
                \() ->
                    Tagger.tags "fox\nsnow"
                        |> List.map Tag.toString
                        |> Expect.equal [ "fox", "snow" ]
            ]
        ]


job : Int -> Tagger.Job
job id =
    { id = id, prompt = "a fox " ++ String.fromInt id }


failure : Tagger.Failure
failure =
    { name = "Error", message = "No" }


run : List Msg -> Tagger.Model -> ( Tagger.Model, Maybe Command, Maybe Tagger.Found )
run msgs model =
    List.foldl
        (\msg ( current, _, _ ) -> Tagger.update msg current)
        ( model, Nothing, Nothing )
        msgs


step : List Msg -> ( State, Maybe Command, Maybe Tagger.Found )
step msgs =
    run msgs Tagger.init
        |> (\( model, command, found ) -> ( model.state, command, found ))
