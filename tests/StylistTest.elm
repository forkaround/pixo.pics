module StylistTest exposing (suite)

import Expect
import Pipeline exposing (Message(..))
import Stylist exposing (Command(..), Event(..), Msg(..), Role(..), State(..), Style(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Stylist"
        [ describe "tidy"
            [ test "drops the empty thinking the model starts with" <|
                \() ->
                    Stylist.tidy "<think>\n\n</think>\n\nSubject: a fox\nMood: calm"
                        |> Expect.equal "Subject: a fox\nMood: calm"
            , test "hides thinking that has not ended yet" <|
                \() ->
                    Stylist.tidy "<think>\nthe user wants"
                        |> Expect.equal ""
            , test "drops a stray closing tag" <|
                \() ->
                    Stylist.tidy "Subject: a fox\nMood: calm </think>"
                        |> Expect.equal "Subject: a fox\nMood: calm"
            , test "keeps the labelled lines once each, in the order they came" <|
                \() ->
                    Stylist.tidy "Here is your prompt:\n\nSubject: a fox\nStyle: photo\nSubject: a fox again\nMood: calm\nEnjoy!"
                        |> Expect.equal "Subject: a fox\nStyle: photo\nMood: calm"
            , test "puts each label on its own line" <|
                \() ->
                    Stylist.tidy "Subject: a lighthouse. Style: watercolor. Setting: a cliff. Mood: calm."
                        |> Expect.equal "Subject: a lighthouse.\nStyle: watercolor.\nSetting: a cliff.\nMood: calm."
            , test "drops bold labels and quotes around the whole answer" <|
                \() ->
                    Stylist.tidy "\"Subject: a fox\n**Style:** photo\nMood: calm\""
                        |> Expect.equal "Subject: a fox\nStyle: photo\nMood: calm"
            , test "keeps quoted words at the end of the answer" <|
                \() ->
                    Stylist.tidy "Subject: a sign reading \"OPEN\""
                        |> Expect.equal "Subject: a sign reading \"OPEN\""
            , test "keeps quoted words inside a line" <|
                \() ->
                    Stylist.tidy "Subject: a robot holding a sign reading \"HELLO\"\nMood: calm"
                        |> Expect.equal "Subject: a robot holding a sign reading \"HELLO\"\nMood: calm"
            , test "waits for a line's label to be whole" <|
                \() ->
                    Stylist.tidy "Subject: a fox\nSty"
                        |> Expect.equal "Subject: a fox"
            ]
        , describe "conversation"
            [ test "is the instructions, two examples and the idea" <|
                \() ->
                    Stylist.conversation { style = Photo, idea = "a fox" }
                        |> List.map .role
                        |> Expect.equal [ System, User, Assistant, User, Assistant, User ]
            , test "ends with the idea" <|
                \() ->
                    Stylist.conversation { style = Comic, idea = "a fox" }
                        |> List.reverse
                        |> List.head
                        |> Maybe.map .content
                        |> Expect.equal (Just "a fox")
            , test "names the style in the instructions" <|
                \() ->
                    Stylist.conversation { style = PixelArt, idea = "a fox" }
                        |> List.head
                        |> Maybe.map (.content >> String.contains "\"Pixel art\"")
                        |> Expect.equal (Just True)
            , test "shows every style answered in six labelled lines" <|
                \() ->
                    Stylist.styles
                        |> List.concatMap (\style -> Stylist.conversation { style = style, idea = "a fox" })
                        |> List.filter (\turn -> turn.role == Assistant)
                        |> List.map (\turn -> List.map (String.split ":" >> List.head) (String.lines turn.content))
                        |> List.filter ((/=) (List.map Just [ "Subject", "Style", "Setting", "Lighting", "Camera", "Mood" ]))
                        |> Expect.equal []
            , test "has examples of a fair length" <|
                \() ->
                    Stylist.styles
                        |> List.concatMap (\style -> Stylist.conversation { style = style, idea = "a fox" })
                        |> List.filter (\turn -> turn.role == Assistant)
                        |> List.map (.content >> String.words >> List.length)
                        |> List.filter (\count -> count < 60 || count > 100)
                        |> Expect.equal []
            , test "has examples that never say what is missing" <|
                \() ->
                    Stylist.styles
                        |> List.concatMap (\style -> Stylist.conversation { style = style, idea = "a fox" })
                        |> List.filter (\turn -> turn.role == Assistant)
                        |> List.concatMap (.content >> String.words)
                        |> List.filter (\word -> List.member (String.toLower word) [ "no", "not", "without", "never" ])
                        |> Expect.equal []
            ]
        , describe "update"
            [ test "loads the model once, then writes with it" <|
                \() ->
                    run [ StyleChosen Photo "a fox", Happened Loaded, Happened (Finished whole), StyleChosen Anime whole ] asleep
                        |> .commands
                        |> List.map commandName
                        |> Expect.equal [ "Load", "Write", "Write" ]
            , test "writes for the last style chosen while it loads" <|
                \() ->
                    run [ StyleChosen Photo "a fox", StyleChosen Comic "a fox", Happened Loaded ] asleep
                        |> .model
                        |> .state
                        |> Expect.equal (Writing { style = Comic, idea = "a fox" } "")
            , test "only loads after a stop while loading" <|
                \() ->
                    run [ StyleChosen Photo "a fox", StopClicked, Happened Loaded ] asleep
                        |> (\result -> ( result.model.state, List.map commandName result.commands ))
                        |> Expect.equal ( Awake, [ "Load" ] )
            , test "ignores text that comes after it finished" <|
                \() ->
                    run [ StyleChosen Photo "a fox", Happened Loaded, Happened (Finished whole), Happened (Streamed "Subject: late") ] asleep
                        |> .prompts
                        |> Expect.equal [ whole ]
            , test "ignores a load nobody waits for" <|
                \() ->
                    run [ Happened Loaded ] asleep
                        |> .model
                        |> .state
                        |> Expect.equal Asleep
            , test "starts again on a failed load" <|
                \() ->
                    run [ StyleChosen Photo "a fox", Happened (Failed { name = "Error", message = "No WebGPU." }) ] asleep
                        |> .model
                        |> (\model -> ( model.state, model.message ))
                        |> Expect.equal ( Asleep, Just (Problem "No WebGPU.") )
            , test "shows nothing when its own stop ends the writing" <|
                \() ->
                    run
                        [ StyleChosen Photo "a fox"
                        , Happened Loaded
                        , StopClicked
                        , Happened (Failed { name = "AbortError", message = "Aborted." })
                        ]
                        asleep
                        |> (\result -> ( result.model.message, result.prompts ))
                        |> Expect.equal ( Nothing, [ "a fox" ] )
            , test "can't undo while it writes" <|
                \() ->
                    run [ StyleChosen Photo "a fox", Happened Loaded, Happened (Finished whole), StyleChosen Anime whole, UndoClicked ] asleep
                        |> .prompts
                        |> Expect.equal [ whole ]
            , test "removes only a model it has" <|
                \() ->
                    run [ RemoveClicked ] asleep
                        |> .commands
                        |> Expect.equal []
            ]
        ]


asleep : Stylist.Model
asleep =
    Stylist.init Nothing


whole : String
whole =
    "Subject: a red fox\nStyle: photograph\nSetting: snow\nLighting: dawn\nCamera: 85mm\nMood: calm"


type alias Run =
    { model : Stylist.Model
    , commands : List Command
    , prompts : List String
    }


run : List Msg -> Stylist.Model -> Run
run msgs model =
    List.foldl
        (\msg done ->
            let
                ( next, command, prompt ) =
                    Stylist.update msg done.model
            in
            { model = next
            , commands = done.commands ++ List.filterMap identity [ command ]
            , prompts = done.prompts ++ List.filterMap identity [ prompt ]
            }
        )
        { model = model, commands = [], prompts = [] }
        msgs


commandName : Command -> String
commandName command =
    case command of
        Load ->
            "Load"

        Write _ ->
            "Write"

        Stop ->
            "Stop"

        Remove ->
            "Remove"
