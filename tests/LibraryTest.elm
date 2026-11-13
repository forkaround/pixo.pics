module LibraryTest exposing (suite)

import Expect
import Idb
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Library exposing (Command(..), Event(..), Msg(..), Notice(..), Save(..))
import Pipeline
import Share
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Library"
        [ describe "pictures"
            [ test "are asked for on the first visit only" <|
                \() ->
                    Library.enter (Library.init False)
                        |> Tuple.mapFirst (Library.enter >> Tuple.second)
                        |> Expect.equal ( [], [ getAll ] )
            , test "are asked for again after they could not be read" <|
                \() ->
                    run [ failed "The database is blocked." ] opening
                        |> Library.enter
                        |> Tuple.second
                        |> Expect.equal [ getAll ]
            , test "come newest first" <|
                \() ->
                    Library.visible loaded
                        |> List.map .id
                        |> Expect.equal [ 3, 2, 1 ]
            , test "leave out one whose settings no picture can have" <|
                \() ->
                    run [ rows [ fox, { lighthouse | width = 700 }, { skyline | prompt = " " } ] ] opening
                        |> Library.visible
                        |> List.map .id
                        |> Expect.equal [ 1 ]
            , test "leave out a row that is not a picture" <|
                \() ->
                    run
                        [ Happened
                            (Answered
                                (Idb.Rows
                                    { label = ""
                                    , rows = [ Encode.object [ ( "id", Encode.int 9 ) ], Encode.string "fox", row lighthouse ]
                                    }
                                )
                            )
                        ]
                        opening
                        |> Library.visible
                        |> List.map .id
                        |> Expect.equal [ 2 ]
            , test "show the picture a row holds at the address the browser gave it" <|
                \() ->
                    Library.visible loaded
                        |> List.map .url
                        |> Expect.equal [ "blob:3", "blob:2", "blob:1" ]
            , test "are named after the model that made them" <|
                \() ->
                    run [ rows [ fox, { lighthouse | model = "someone/else" } ] ] opening
                        |> Library.visible
                        |> List.map Library.modelName
                        |> Expect.equal [ "someone/else", "Bonsai ternary, 2-bit" ]
            ]
        , describe "search"
            [ test "keeps the pictures whose prompt matches, best first" <|
                \() ->
                    run [ QueryChanged "sunset lighthouse" ] loaded
                        |> Library.visible
                        |> List.map .id
                        |> Expect.equal [ 2, 3 ]
            , test "shows everything again without a word to look for" <|
                \() ->
                    run [ QueryChanged "fox", QueryChanged "  " ] loaded
                        |> Library.visible
                        |> List.length
                        |> Expect.equal 3
            , test "finds a picture saved since" <|
                \() ->
                    run [ added "blob:new" { fox | id = 4, prompt = "a ramen bowl" }, QueryChanged "ramen" ] loaded
                        |> Library.visible
                        |> List.map .id
                        |> Expect.equal [ 4 ]
            , test "forgets a picture that was deleted" <|
                \() ->
                    run [ deleted 1, QueryChanged "fox" ] loaded
                        |> Library.visible
                        |> Expect.equal []
            ]
        , describe "saving"
            [ test "marks the picture while it is stored, then as saved" <|
                \() ->
                    List.concatMap
                        (\picture ->
                            [ run [ SaveClicked picture ] loaded
                            , run [ SaveClicked picture, added picture.url fox ] loaded
                            ]
                                |> List.map (Library.saveOf picture.url)
                        )
                        made
                        |> Expect.equal [ Saving, Saved ]
            , test "stores a picture once" <|
                \() ->
                    List.map
                        (\picture ->
                            Library.update (SaveClicked picture) loaded
                                |> Tuple.mapBoth (Library.update (SaveClicked picture) >> Tuple.second >> List.length) List.length
                        )
                        made
                        |> Expect.equal [ ( 0, 1 ) ]
            , test "stores what made the picture, and the picture from its address" <|
                \() ->
                    List.concatMap (\picture -> Tuple.second (Library.update (SaveClicked picture) loaded)) made
                        |> List.map
                            (\command ->
                                case command of
                                    Database (Idb.Add request) ->
                                        { database = request.database.name
                                        , store = request.store
                                        , row = Encode.encode 0 (Encode.object request.row)
                                        , blobs = request.blobs
                                        , label = request.label
                                        }

                                    _ ->
                                        { database = "", store = "", row = "", blobs = [], label = "" }
                            )
                        |> Expect.equal
                            [ { database = "pixo"
                              , store = "pictures"
                              , row = "{\"prompt\":\"a red fox in the snow\",\"width\":512,\"height\":512,\"steps\":4,\"seed\":578021833,\"model\":\"prism-ml/bonsai-image-ternary-4B-mlx-2bit\"}"
                              , blobs = [ ( "blob", "blob:studio" ) ]
                              , label = "blob:studio"
                              }
                            ]
            , test "lets a picture be saved again after it could not be stored" <|
                \() ->
                    List.map
                        (\picture ->
                            run [ SaveClicked picture, failed "The disk is full." ] loaded
                                |> (\model -> ( Library.saveOf picture.url model, model.notice ))
                        )
                        made
                        |> Expect.equal [ ( Unsaved, Just (Problem "The disk is full.") ) ]
            , test "reads the pictures again when one is saved while they load" <|
                \() ->
                    Library.update (added "blob:studio" fox) opening
                        |> Tuple.second
                        |> Expect.equal [ getAll ]
            ]
        , describe "deleting"
            [ test "asks first" <|
                \() ->
                    Library.update (DeleteRequested 1) loaded
                        |> Tuple.second
                        |> Expect.equal []
            , test "deletes nothing when that is cancelled" <|
                \() ->
                    run [ DeleteRequested 1, DeleteCancelled ] loaded
                        |> .confirming
                        |> Expect.equal Nothing
            , test "deletes the row with the picture's id once that is confirmed" <|
                \() ->
                    Library.update (DeleteConfirmed 2) loaded
                        |> Tuple.second
                        |> List.map
                            (\command ->
                                case command of
                                    Database (Idb.Delete request) ->
                                        ( request.store, Decode.decodeValue Decode.int request.key )

                                    _ ->
                                        ( "", Ok 0 )
                            )
                        |> Expect.equal [ ( "pictures", Ok 2 ) ]
            , test "gives the picture's address back once its row is gone" <|
                \() ->
                    Library.update (deleted 2) loaded
                        |> Tuple.second
                        |> Expect.equal [ Database (Idb.Release [ "blob:2" ]) ]
            , test "keeps a picture whose row could not be deleted" <|
                \() ->
                    run [ DeleteConfirmed 2, failed "The database is blocked." ] loaded
                        |> (\model -> ( List.map .id (Library.visible model), model.notice ))
                        |> Expect.equal ( [ 3, 2, 1 ], Just (Problem "The database is blocked.") )
            , test "closes the picture that is open once it is gone" <|
                \() ->
                    run [ Opened 1, deleted 1 ] loaded
                        |> .zoomed
                        |> Expect.equal False
            , test "opens the next picture without the details the last one had pinned" <|
                \() ->
                    run [ Opened 1, DetailsToggled, Closed, Opened 2 ] loaded
                        |> .detailed
                        |> Expect.equal False
            , test "steps to the picture beside the open one, and stops at either end" <|
                \() ->
                    [ [ Opened 2, Stepped 1 ]
                    , [ Opened 2, Stepped -1 ]
                    , [ Opened 1, Stepped 1 ]
                    , [ Opened 3, Stepped -1 ]
                    , [ Opened 2, Closed, Stepped 1 ]
                    ]
                        |> List.map (\msgs -> (run msgs loaded).viewing)
                        |> Expect.equal [ Just 1, Just 3, Just 1, Just 3, Just 2 ]
            , test "keeps the details pinned and drops the question while stepping" <|
                \() ->
                    run [ Opened 2, DetailsToggled, DeleteRequested 2, Stepped 1 ] loaded
                        |> (\model -> ( model.detailed, model.confirming ))
                        |> Expect.equal ( True, Nothing )
            , test "leaves another picture open" <|
                \() ->
                    run [ Opened 2, deleted 1 ] loaded
                        |> .zoomed
                        |> Expect.equal True
            ]
        , describe "sharing"
            [ test "copies the picture, then opens the network with the prompt" <|
                \() ->
                    Library.update (PostClicked Share.Bluesky 1) loaded
                        |> Tuple.second
                        |> List.map
                            (\command ->
                                case command of
                                    Copy copied ->
                                        copied.url

                                    Open url ->
                                        url

                                    _ ->
                                        ""
                            )
                        |> Expect.equal [ "blob:1", "https://bsky.app/intent/compose?text=a%20red%20fox%20in%20the%20snow" ]
            , test "says so when the picture is copied" <|
                \() ->
                    run [ Happened Copied ] loaded
                        |> .notice
                        |> Expect.equal (Just (Info "Picture copied."))
            , test "says nothing when the share sheet is closed" <|
                \() ->
                    run [ Happened (ShareFailed "AbortError") ] loaded
                        |> .notice
                        |> Expect.equal Nothing
            , test "says so when the browser refuses" <|
                \() ->
                    run [ Happened (ShareFailed "NotAllowedError") ] loaded
                        |> .notice
                        |> Expect.equal (Just (Problem "Couldn't share the picture. Download it instead."))
            ]
        ]


opening : Library.Model
opening =
    Library.enter (Library.init False)
        |> Tuple.first


loaded : Library.Model
loaded =
    run [ rows [ fox, lighthouse, skyline ] ] opening


run : List Msg -> Library.Model -> Library.Model
run msgs model =
    List.foldl (\msg -> Library.update msg >> Tuple.first) model msgs


made : List Pipeline.Picture
made =
    List.foldl (\msg -> Pipeline.update msg >> Tuple.first)
        (Tuple.first (Pipeline.enter (Pipeline.init 1 True [])))
        [ Pipeline.Happened (Pipeline.SupportChecked (Just [ "shader-f16" ]))
        , Pipeline.PromptChanged "a red fox in the snow"
        , Pipeline.GenerateClicked
        , Pipeline.Happened Pipeline.ModelLoaded
        , Pipeline.Happened (Pipeline.PictureMade "blob:studio")
        ]
        |> .pictures


getAll : Command
getAll =
    Database
        (Idb.GetAll
            { database =
                { name = "pixo"
                , version = 1
                , stores = [ { name = "pictures", keyPath = "id", autoIncrement = True } ]
                }
            , store = "pictures"
            , label = ""
            }
        )


rows : List Row -> Msg
rows stored =
    Happened (Answered (Idb.Rows { label = "", rows = List.map row stored }))


added : String -> Row -> Msg
added source stored =
    Happened (Answered (Idb.Added { label = source, row = row stored }))


deleted : Int -> Msg
deleted id =
    Happened (Answered (Idb.Deleted { label = "", key = Encode.int id }))


failed : String -> Msg
failed message =
    Happened (Answered (Idb.Failed { label = "", message = message }))


type alias Row =
    { id : Int
    , blob : String
    , prompt : String
    , width : Int
    , height : Int
    , steps : Int
    , seed : Int
    , model : String
    }


row : Row -> Value
row stored =
    Encode.object
        [ ( "id", Encode.int stored.id )
        , ( "blob", Encode.string stored.blob )
        , ( "prompt", Encode.string stored.prompt )
        , ( "width", Encode.int stored.width )
        , ( "height", Encode.int stored.height )
        , ( "steps", Encode.int stored.steps )
        , ( "seed", Encode.int stored.seed )
        , ( "model", Encode.string stored.model )
        ]


fox : Row
fox =
    { id = 1
    , blob = "blob:1"
    , prompt = "a red fox in the snow"
    , width = 512
    , height = 512
    , steps = 4
    , seed = 7
    , model = "prism-ml/bonsai-image-ternary-4B-mlx-2bit"
    }


lighthouse : Row
lighthouse =
    { fox | id = 2, blob = "blob:2", prompt = "a lighthouse on a cliff at sunset", width = 1024, height = 576 }


skyline : Row
skyline =
    { fox | id = 3, blob = "blob:3", prompt = "a city skyline at sunset", width = 576, height = 1024 }
