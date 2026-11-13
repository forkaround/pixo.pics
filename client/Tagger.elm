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
                    case tags text of
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
        [ "You sort pictures in a personal library by tags."
        , "You are given the prompt a picture was made from. Answer with 4 to 8 tags on one line, separated by commas: the main subject first, then other things in the picture, the place, the art style and the mood."
        , "Each tag is one or two lowercase English words, a noun or an adjective."
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
      , "fisherman, old man, rowboat, mountain lake, forest, dawn, photo, peaceful"
      )
    , ( "a corgi in a red cape flying over the city at night, comic book style"
      , "dog, corgi, superhero, cape, city, night, comic, funny"
      )
    ]


tags : String -> List Tag
tags answer =
    Stylist.withoutThinking answer
        |> String.replace "\n" ","
        |> String.split ","
        |> List.filterMap Tag.fromString
        |> List.foldl
            (\tag kept ->
                if List.member tag kept then
                    kept

                else
                    kept ++ [ tag ]
            )
            []
        |> List.take 8
