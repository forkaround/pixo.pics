module Tagger exposing
    ( Command(..)
    , Event(..)
    , Failure
    , Found
    , Job
    , Model
    , Msg(..)
    , State(..)
    , conversation
    , init
    , isAsleep
    , pending
    , request
    , tags
    , update
    )

import Data.Tag as Tag exposing (Tag)
import Stylist exposing (Request, Role(..), Turn)


type alias Model =
    { state : State
    , queue : List Job
    , tried : List Int
    }


type State
    = Asleep
    | Loading
    | Tagging Job
    | Releasing


type alias Job =
    { id : Int
    , prompt : String
    }


type alias Found =
    { id : Int
    , tags : List Tag
    }


type alias Failure =
    { name : String
    , message : String
    }


type Msg
    = Asked (List Job)
    | Happened Event


type Event
    = Loaded
    | Finished String
    | Released
    | Failed Failure


type Command
    = Load
    | Tag Request
    | Release


init : Model
init =
    { state = Asleep, queue = [], tried = [] }


isAsleep : Model -> Bool
isAsleep model =
    model.state == Asleep


pending : Model -> Int
pending model =
    List.length model.queue
        + (case model.state of
            Tagging _ ->
                1

            _ ->
                0
          )


update : Msg -> Model -> ( Model, Maybe Command, Maybe Found )
update msg model =
    case msg of
        Asked jobs ->
            let
                fresh =
                    List.foldl
                        (\job kept ->
                            if List.member job.id model.tried || List.any (\known -> known.id == job.id) kept then
                                kept

                            else
                                kept ++ [ job ]
                        )
                        []
                        jobs

                queued =
                    { model | queue = model.queue ++ fresh, tried = model.tried ++ List.map .id fresh }
            in
            if queued.state == Asleep && not (List.isEmpty queued.queue) then
                ( { queued | state = Loading }, Just Load, Nothing )

            else
                ( queued, Nothing, Nothing )

        Happened Loaded ->
            case model.state of
                Loading ->
                    next model Nothing

                _ ->
                    ( model, Nothing, Nothing )

        Happened (Finished text) ->
            case model.state of
                Tagging job ->
                    case tags job.prompt text of
                        [] ->
                            next model Nothing

                        found ->
                            next model (Just { id = job.id, tags = found })

                _ ->
                    ( model, Nothing, Nothing )

        Happened Released ->
            case model.state of
                Releasing ->
                    released model

                _ ->
                    ( model, Nothing, Nothing )

        Happened (Failed _) ->
            case model.state of
                Loading ->
                    ( { model | state = Asleep, queue = [] }, Nothing, Nothing )

                Tagging _ ->
                    next model Nothing

                Releasing ->
                    released model

                Asleep ->
                    ( model, Nothing, Nothing )


next : Model -> Maybe Found -> ( Model, Maybe Command, Maybe Found )
next model found =
    case model.queue of
        job :: rest ->
            ( { model | state = Tagging job, queue = rest }, Just (Tag (request job)), found )

        [] ->
            ( { model | state = Releasing }, Just Release, found )


released : Model -> ( Model, Maybe Command, Maybe Found )
released model =
    if List.isEmpty model.queue then
        ( { model | state = Asleep }, Nothing, Nothing )

    else
        ( { model | state = Loading }, Just Load, Nothing )


request : Job -> Request
request job =
    { model = Stylist.modelId
    , messages = conversation job
    , temperature = 0.2
    , topP = 0.9
    , maxTokens = 64
    , thinking = False
    }


conversation : Job -> List Turn
conversation job =
    { role = System, content = instructions }
        :: List.concatMap
            (\( prompt, answer ) ->
                [ { role = User, content = prompt }
                , { role = Assistant, content = answer }
                ]
            )
            examples
        ++ [ { role = User, content = job.prompt } ]


instructions : String
instructions =
    String.join "\n"
        [ "You tag pictures in a personal library so they can be found later."
        , "You are given the prompt a picture was made from. Answer with 3 to 6 tags on one line, separated by commas: the main subject first, then the most important things the prompt names, the place and the art style."
        , "Take every tag from the prompt: never add a thing, a place or a mood it doesn't name. Leave out cameras, lenses, film stocks and lighting."
        , "Each tag is one or two lowercase English words. Translate a prompt in another language."
        , "Answer with the tags and nothing else."
        ]


examples : List ( String, String )
examples =
    [ ( String.join "\n"
            [ "Subject: an old fisherman with a grey beard and a weathered face casting a line from a wooden rowboat, his hands gripping the rod"
            , "Style: documentary photograph, natural colors, fine detail in skin and fabric"
            , "Setting: a still mountain lake, pine forest on the far shore, thin mist over the water"
            , "Lighting: first light of dawn, soft warm sun low behind the trees, gentle rim light on his face"
            , "Camera: shot on Fujifilm X-T5 with a 35mm f/1.4 lens, eye level, shallow depth of field"
            , "Mood: quiet, patient, peaceful"
            ]
      , "fisherman, rowboat, mountain lake, forest, dawn, photo"
      )
    , ( "a glass teapot of green tea on a wooden table, watercolor"
      , "teapot, green tea, table, watercolor"
      )
    , ( "una casa roja junto al mar al atardecer"
      , "house, sea, sunset"
      )
    ]


borrowed : String -> Tag -> Bool
borrowed prompt tag =
    List.member tag shown && not (String.contains (Tag.toString tag) (String.toLower prompt))


shown : List Tag
shown =
    List.concatMap (Tuple.second >> String.split "," >> List.filterMap Tag.fromString) examples


tags : String -> String -> List Tag
tags prompt answer =
    Stylist.withoutThinking answer
        |> String.replace "\n" ","
        |> String.split ","
        |> List.filterMap Tag.fromString
        |> List.filter (not << borrowed prompt)
        |> List.foldl
            (\tag kept ->
                if List.member tag kept then
                    kept

                else
                    kept ++ [ tag ]
            )
            []
        |> List.take 8
