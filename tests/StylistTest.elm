module StylistTest exposing (suite)

import Expect
import Pipeline exposing (Message(..))
import Stylist exposing (Command(..), Event(..), Handover(..), Msg(..), Role(..), State(..), Style(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Stylist"
        [ test "needs WebGPU only to write a prompt" <|
            \() ->
                List.map Stylist.needsWebGpu [ StyleChosen Photo square "a fox", StopClicked, UndoClicked, RemoveClicked ]
                    |> Expect.equal [ True, False, False, False ]
        , describe "tidy"
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
                    Stylist.conversation { style = Photo, frame = square, idea = "a fox" }
                        |> List.map .role
                        |> Expect.equal [ System, User, Assistant, User, Assistant, User ]
            , test "ends with the idea" <|
                \() ->
                    Stylist.conversation { style = Comic, frame = square, idea = "a fox" }
                        |> List.reverse
                        |> List.head
                        |> Maybe.map .content
                        |> Expect.equal (Just "a fox")
            , test "names the style in the instructions" <|
                \() ->
                    Stylist.conversation { style = PixelArt, frame = square, idea = "a fox" }
                        |> List.head
                        |> Maybe.map (.content >> String.contains "\"Pixel art\"")
                        |> Expect.equal (Just True)
            , test "shows every style answered in six labelled lines" <|
                \() ->
                    Stylist.styles
                        |> List.concatMap (\style -> Stylist.conversation { style = style, frame = square, idea = "a fox" })
                        |> List.filter (\turn -> turn.role == Assistant)
                        |> List.map (\turn -> List.map (String.split ":" >> List.head) (String.lines turn.content))
                        |> List.filter ((/=) (List.map Just [ "Subject", "Style", "Setting", "Lighting", "Camera", "Mood" ]))
                        |> Expect.equal []
            , test "tells the orientation of the picture, and no size or ratio to copy" <|
                \() ->
                    List.map
                        (\frame ->
                            Stylist.conversation { style = Photo, frame = frame, idea = "a fox" }
                                |> List.head
                                |> Maybe.map .content
                                |> Maybe.withDefault ""
                                |> (\content -> List.filter (\words -> String.contains words content) [ "a square frame", "a tall portrait frame", "a wide landscape frame", "1:1", "9:16", "16:9", "pixels", "1024" ])
                        )
                        [ square, { width = 576, height = 1024 }, { width = 1024, height = 576 } ]
                        |> Expect.equal [ [ "a square frame" ], [ "a tall portrait frame" ], [ "a wide landscape frame" ] ]
            , test "has examples that frame no shape of their own" <|
                \() ->
                    Stylist.styles
                        |> List.concatMap (\style -> Stylist.conversation { style = style, frame = square, idea = "a fox" })
                        |> List.filter (\turn -> turn.role == Assistant)
                        |> List.filter (\turn -> String.contains ":1 " turn.content || String.contains ":1," turn.content || String.contains "widescreen" turn.content)
                        |> Expect.equal []
            , test "has examples of a fair length" <|
                \() ->
                    Stylist.styles
                        |> List.concatMap (\style -> Stylist.conversation { style = style, frame = square, idea = "a fox" })
                        |> List.filter (\turn -> turn.role == Assistant)
                        |> List.map (.content >> String.words >> List.length)
                        |> List.filter (\count -> count < 60 || count > 100)
                        |> Expect.equal []
            , test "has examples that never say what is missing" <|
                \() ->
                    Stylist.styles
                        |> List.concatMap (\style -> Stylist.conversation { style = style, frame = square, idea = "a fox" })
                        |> List.filter (\turn -> turn.role == Assistant)
                        |> List.concatMap (.content >> String.words)
                        |> List.filter (\word -> List.member (String.toLower word) [ "no", "not", "without", "never" ])
                        |> Expect.equal []
            ]
        , describe "update"
            [ test "loads the model once, then writes with it" <|
                \() ->
                    run [ StyleChosen Photo square "a fox", Happened Loaded, Happened (Finished whole), StyleChosen Anime square whole ] asleep
                        |> .commands
                        |> List.map commandName
                        |> Expect.equal [ "Load", "Write", "Write" ]
            , test "writes for the last style chosen while it loads" <|
                \() ->
                    run [ StyleChosen Photo square "a fox", StyleChosen Comic square "a fox", Happened Loaded ] asleep
                        |> .model
                        |> .state
                        |> Expect.equal (Writing { style = Comic, frame = square, idea = "a fox" } "")
            , test "only loads after a stop while loading" <|
                \() ->
                    run [ StyleChosen Photo square "a fox", StopClicked, Happened Loaded ] asleep
                        |> (\result -> ( result.model.state, List.map commandName result.commands ))
                        |> Expect.equal ( Awake, [ "Load" ] )
            , test "ignores text that comes after it finished" <|
                \() ->
                    run [ StyleChosen Photo square "a fox", Happened Loaded, Happened (Finished whole), Happened (Streamed "Subject: late") ] asleep
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
                    run [ StyleChosen Photo square "a fox", Happened (Failed { name = "Error", message = "No WebGPU." }) ] asleep
                        |> .model
                        |> (\model -> ( model.state, model.message ))
                        |> Expect.equal ( Asleep, Just (Problem "No WebGPU.") )
            , test "shows nothing when its own stop ends the writing" <|
                \() ->
                    run
                        [ StyleChosen Photo square "a fox"
                        , Happened Loaded
                        , StopClicked
                        , Happened (Failed { name = "AbortError", message = "Aborted." })
                        ]
                        asleep
                        |> (\result -> ( result.model.message, result.prompts ))
                        |> Expect.equal ( Nothing, [ "a fox" ] )
            , test "can't undo while it writes" <|
                \() ->
                    run [ StyleChosen Photo square "a fox", Happened Loaded, Happened (Finished whole), StyleChosen Anime square whole, UndoClicked ] asleep
                        |> .prompts
                        |> Expect.equal [ whole ]
            , test "takes the prompt while it writes, and gives it back" <|
                \() ->
                    run [ StyleChosen Photo square "a fox", Happened Loaded, Happened (Finished whole) ] asleep
                        |> .handovers
                        |> Expect.equal [ Took, Gave whole ]
            , test "gives the idea back when it stops loading" <|
                \() ->
                    run [ StyleChosen Photo square "a fox", StopClicked ] asleep
                        |> .handovers
                        |> Expect.equal [ Took, Gave "a fox" ]
            , test "gives the idea back when the load fails" <|
                \() ->
                    run [ StyleChosen Photo square "a fox", Happened (Failed { name = "Error", message = "No WebGPU." }) ] asleep
                        |> .handovers
                        |> Expect.equal [ Took, Gave "a fox" ]
            , test "takes no prompt it can't rewrite" <|
                \() ->
                    run [ StyleChosen Photo square "  " ] asleep
                        |> .handovers
                        |> Expect.equal []
            , test "keeps the prompt to itself while the answer streams in" <|
                \() ->
                    run [ StyleChosen Photo square "a fox", Happened Loaded, Happened (Streamed "Subject: a red fox") ] asleep
                        |> (\result -> ( result.handovers, Stylist.shown result.model ))
                        |> Expect.equal ( [ Took ], Just "Subject: a red fox" )
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
    , handovers : List Handover
    , prompts : List String
    }


run : List Msg -> Stylist.Model -> Run
run msgs model =
    List.foldl
        (\msg done ->
            let
                ( next, command, handover ) =
                    Stylist.update msg done.model
            in
            { model = next
            , commands = done.commands ++ List.filterMap identity [ command ]
            , handovers = done.handovers ++ List.filterMap identity [ handover ]
            , prompts = done.prompts ++ List.filterMap given [ handover ]
            }
        )
        { model = model, commands = [], handovers = [], prompts = [] }
        msgs


given : Maybe Handover -> Maybe String
given handover =
    case handover of
        Just (Gave prompt) ->
            Just prompt

        _ ->
            Nothing


square : Stylist.Frame
square =
    { width = 1024, height = 1024 }


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
