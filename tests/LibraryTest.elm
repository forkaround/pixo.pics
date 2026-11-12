module LibraryTest exposing (suite)

import Expect
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
                        |> Expect.equal ( [], [ Load ] )
            , test "are asked for again after they could not be read" <|
                \() ->
                    run [ Happened (Failed "The database is blocked.") ] opening
                        |> Library.enter
                        |> Tuple.second
                        |> Expect.equal [ Load ]
            , test "come newest first" <|
                \() ->
                    Library.visible loaded
                        |> List.map .id
                        |> Expect.equal [ 3, 2, 1 ]
            , test "leave out one whose settings no picture can have" <|
                \() ->
                    run [ Happened (PicturesLoaded [ fox, { lighthouse | width = 700 }, { skyline | prompt = " " } ]) ] opening
                        |> Library.visible
                        |> List.map .id
                        |> Expect.equal [ 1 ]
            , test "are named after the model that made them" <|
                \() ->
                    run [ Happened (PicturesLoaded [ fox, { lighthouse | model = "someone/else" } ]) ] opening
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
                    run [ Happened (PictureSaved "blob:new" { fox | id = 4, prompt = "a ramen bowl" }), QueryChanged "ramen" ] loaded
                        |> Library.visible
                        |> List.map .id
                        |> Expect.equal [ 4 ]
            , test "forgets a picture that was deleted" <|
                \() ->
                    run [ Happened (PictureDeleted 1), QueryChanged "fox" ] loaded
                        |> Library.visible
                        |> Expect.equal []
            ]
        , describe "saving"
            [ test "marks the picture while it is stored, then as saved" <|
                \() ->
                    List.concatMap
                        (\picture ->
                            [ run [ SaveClicked picture ] loaded
                            , run [ SaveClicked picture, Happened (PictureSaved picture.url fox) ] loaded
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
            , test "lets a picture be saved again after it could not be stored" <|
                \() ->
                    List.map
                        (\picture ->
                            run [ SaveClicked picture, Happened (Failed "The disk is full.") ] loaded
                                |> (\model -> ( Library.saveOf picture.url model, model.notice ))
                        )
                        made
                        |> Expect.equal [ ( Unsaved, Just (Problem "The disk is full.") ) ]
            , test "reads the pictures again when one is saved while they load" <|
                \() ->
                    Library.update (Happened (PictureSaved "blob:studio" fox)) opening
                        |> Tuple.second
                        |> Expect.equal [ Load ]
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
            , test "closes the picture that is open once it is gone" <|
                \() ->
                    run [ Opened 1, Happened (PictureDeleted 1) ] loaded
                        |> .zoomed
                        |> Expect.equal False
            , test "opens the next picture without the details the last one had pinned" <|
                \() ->
                    run [ Opened 1, DetailsToggled, Closed, Opened 2 ] loaded
                        |> .detailed
                        |> Expect.equal False
            , test "leaves another picture open" <|
                \() ->
                    run [ Opened 2, Happened (PictureDeleted 1) ] loaded
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
    run [ Happened (PicturesLoaded [ fox, lighthouse, skyline ]) ] opening


run : List Msg -> Library.Model -> Library.Model
run msgs model =
    List.foldl (\msg -> Library.update msg >> Tuple.first) model msgs


made : List Pipeline.Picture
made =
    List.foldl (\msg -> Pipeline.update msg >> Tuple.first)
        (Tuple.first (Pipeline.enter (Pipeline.init 1 [])))
        [ Pipeline.Happened (Pipeline.SupportChecked True)
        , Pipeline.PromptChanged "a red fox in the snow"
        , Pipeline.GenerateClicked
        , Pipeline.Happened Pipeline.ModelLoaded
        , Pipeline.Happened (Pipeline.PictureMade "blob:studio")
        ]
        |> .pictures


fox : Library.Stored
fox =
    { id = 1
    , url = "blob:1"
    , prompt = "a red fox in the snow"
    , width = 512
    , height = 512
    , steps = 4
    , seed = 7
    , model = "prism-ml/bonsai-image-ternary-4B-mlx-2bit"
    }


lighthouse : Library.Stored
lighthouse =
    { fox | id = 2, url = "blob:2", prompt = "a lighthouse on a cliff at sunset", width = 1024, height = 576 }


skyline : Library.Stored
skyline =
    { fox | id = 3, url = "blob:3", prompt = "a city skyline at sunset", width = 576, height = 1024 }
