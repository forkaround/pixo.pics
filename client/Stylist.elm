module Stylist exposing
    ( Command(..)
    , Event(..)
    , Failure
    , Job
    , Model
    , Msg(..)
    , Request
    , Role(..)
    , State(..)
    , Style(..)
    , Turn
    , canUndo
    , conversation
    , init
    , isBusy
    , modelId
    , modelSize
    , needsWebGpu
    , request
    , styleName
    , styles
    , tidy
    , update
    , withoutThinking
    )

import Data.Prompt as Prompt
import Pipeline exposing (Message(..))



-- MODEL


type alias Model =
    { state : State
    , downloaded : Bool
    , written : Maybe Written
    , message : Maybe Message
    }


type State
    = Asleep
    | Loading Float (Maybe Job)
    | Awake
    | Writing Job String
    | Stopping Job
    | Removing


type alias Job =
    { style : Style
    , idea : String
    }


type alias Written =
    { idea : String
    , prompt : String
    }


type Style
    = Photo
    | Cinematic
    | AnalogFilm
    | Anime
    | Watercolor
    | OilPainting
    | Render
    | PixelArt
    | Comic
    | ProductShot


type alias Turn =
    { role : Role
    , content : String
    }


type Role
    = System
    | User
    | Assistant


type alias Request =
    { model : String
    , messages : List Turn
    , temperature : Float
    , topP : Float
    , maxTokens : Int
    , thinking : Bool
    }


type alias Failure =
    { name : String
    , message : String
    }


init : Maybe String -> Model
init stored =
    { state = Asleep
    , downloaded = stored == Just modelId
    , written = Nothing
    , message = Nothing
    }


modelId : String
modelId =
    "Qwen3-1.7B-q4f16_1-MLC"


modelSize : String
modelSize =
    "984 MB"


styles : List Style
styles =
    [ Photo, Cinematic, AnalogFilm, Anime, Watercolor, OilPainting, Render, PixelArt, Comic, ProductShot ]


styleName : Style -> String
styleName style =
    case style of
        Photo ->
            "Photo"

        Cinematic ->
            "Cinematic"

        AnalogFilm ->
            "Analog film"

        Anime ->
            "Anime"

        Watercolor ->
            "Watercolor"

        OilPainting ->
            "Oil painting"

        Render ->
            "3D render"

        PixelArt ->
            "Pixel art"

        Comic ->
            "Comic"

        ProductShot ->
            "Product shot"


isBusy : Model -> Bool
isBusy model =
    case model.state of
        Loading _ (Just _) ->
            True

        Writing _ _ ->
            True

        Stopping _ ->
            True

        _ ->
            False


canUndo : String -> Model -> Bool
canUndo prompt model =
    case model.written of
        Just written ->
            written.prompt == prompt && not (isBusy model)

        Nothing ->
            False



-- UPDATE


type Msg
    = StyleChosen Style String
    | StopClicked
    | UndoClicked
    | RemoveClicked
    | Happened Event


type Event
    = Progressed Float
    | Loaded
    | Streamed String
    | Finished String
    | Removed
    | Failed Failure


type Command
    = Load
    | Write Request
    | Stop
    | Remove


update : Msg -> Model -> ( Model, Maybe Command, Maybe String )
update msg model =
    case msg of
        StyleChosen style prompt ->
            case Prompt.parse prompt of
                Nothing ->
                    ( { model | message = Just (Problem "Describe the picture first.") }, Nothing, Nothing )

                Just _ ->
                    start { style = style, idea = ideaOf prompt model } { model | message = Nothing }

        StopClicked ->
            case model.state of
                Loading progress (Just _) ->
                    ( { model | state = Loading progress Nothing }, Nothing, Nothing )

                Writing job _ ->
                    ( { model | state = Stopping job }, Just Stop, Nothing )

                _ ->
                    ( model, Nothing, Nothing )

        UndoClicked ->
            case ( model.written, isBusy model ) of
                ( Just written, False ) ->
                    ( { model | written = Nothing, message = Nothing }, Nothing, Just written.idea )

                _ ->
                    ( model, Nothing, Nothing )

        RemoveClicked ->
            case model.state of
                Asleep ->
                    remove model

                Awake ->
                    remove model

                _ ->
                    ( model, Nothing, Nothing )

        Happened event ->
            happened event model


ideaOf : String -> Model -> String
ideaOf prompt model =
    case model.written of
        Just written ->
            if written.prompt == prompt then
                written.idea

            else
                String.trim prompt

        Nothing ->
            String.trim prompt


start : Job -> Model -> ( Model, Maybe Command, Maybe String )
start job model =
    case model.state of
        Asleep ->
            ( { model | state = Loading 0 (Just job) }, Just Load, Nothing )

        Loading progress _ ->
            ( { model | state = Loading progress (Just job) }, Nothing, Nothing )

        Awake ->
            write job model

        _ ->
            ( model, Nothing, Nothing )


write : Job -> Model -> ( Model, Maybe Command, Maybe String )
write job model =
    ( { model | state = Writing job "" }, Just (Write (request job)), Nothing )


remove : Model -> ( Model, Maybe Command, Maybe String )
remove model =
    if model.downloaded then
        ( { model | state = Removing, message = Nothing }, Just Remove, Nothing )

    else
        ( model, Nothing, Nothing )


happened : Event -> Model -> ( Model, Maybe Command, Maybe String )
happened event model =
    case ( event, model.state ) of
        ( Progressed progress, Loading _ waiting ) ->
            ( { model | state = Loading progress waiting }, Nothing, Nothing )

        ( Loaded, Loading _ waiting ) ->
            let
                loaded =
                    { model | state = Awake, downloaded = True }
            in
            case waiting of
                Just job ->
                    write job loaded

                Nothing ->
                    ( loaded, Nothing, Nothing )

        ( Streamed text, Writing job _ ) ->
            ( { model | state = Writing job text }, Nothing, Just (tidy text) )

        ( Finished text, Writing job _ ) ->
            let
                prompt =
                    tidy text
            in
            if isWhole prompt then
                ( { model | state = Awake, written = Just { idea = job.idea, prompt = prompt } }, Nothing, Just prompt )

            else
                ( { model | state = Awake, message = Just (Problem "That didn't come out right. Try again.") }, Nothing, Just job.idea )

        ( Finished _, Stopping job ) ->
            ( { model | state = Awake }, Nothing, Just job.idea )

        ( Removed, Removing ) ->
            ( { model | state = Asleep, downloaded = False, message = Just (Info "Prompt model removed.") }, Nothing, Nothing )

        ( Failed failure, state ) ->
            failed failure state model

        _ ->
            ( model, Nothing, Nothing )


failed : Failure -> State -> Model -> ( Model, Maybe Command, Maybe String )
failed failure state model =
    let
        problem =
            Just (Problem (explain failure))
    in
    case state of
        Loading _ _ ->
            ( { model | state = Asleep, message = problem }, Nothing, Nothing )

        Writing job _ ->
            ( { model | state = Awake, message = problem }, Nothing, Just job.idea )

        Stopping job ->
            ( { model | state = Awake }, Nothing, Just job.idea )

        Removing ->
            ( { model | state = Asleep, message = problem }, Nothing, Nothing )

        _ ->
            ( model, Nothing, Nothing )


explain : Failure -> String
explain failure =
    if String.isEmpty (String.trim failure.message) then
        "Something went wrong: " ++ failure.name ++ "."

    else
        failure.message



-- PROMPT


labels : List String
labels =
    [ "Subject", "Style", "Setting", "Lighting", "Camera", "Mood" ]


tidy : String -> String
tidy raw =
    withoutThinking raw
        |> String.replace "**" ""
        |> String.trim
        |> unquote
        |> splitLabels
        |> String.lines
        |> List.map String.trim
        |> List.foldl keepLabelled ( [], [] )
        |> Tuple.second
        |> List.reverse
        |> String.join "\n"


withoutThinking : String -> String
withoutThinking raw =
    case String.split "<think>" raw of
        first :: rest ->
            (first :: List.map afterThinking rest)
                |> String.concat
                |> String.replace "</think>" ""

        [] ->
            raw


afterThinking : String -> String
afterThinking part =
    case String.split "</think>" part of
        _ :: after :: more ->
            String.join "</think>" (after :: more)

        _ ->
            ""


splitLabels : String -> String
splitLabels text =
    List.foldl
        (\label done -> String.replace (" " ++ label ++ ":") ("\n" ++ label ++ ":") done)
        text
        (List.drop 1 labels)


unquote : String -> String
unquote text =
    if String.startsWith "\"" text then
        let
            rest =
                String.dropLeft 1 text
        in
        if String.endsWith "\"" rest && modBy 2 (List.length (String.indexes "\"" rest)) == 1 then
            String.dropRight 1 rest

        else
            rest

    else
        text


keepLabelled : String -> ( List String, List String ) -> ( List String, List String )
keepLabelled line ( seen, kept ) =
    case labelOf line of
        Just label ->
            if List.member label seen then
                ( seen, kept )

            else
                ( label :: seen, line :: kept )

        Nothing ->
            ( seen, kept )


labelOf : String -> Maybe String
labelOf line =
    List.filter (\label -> String.startsWith (label ++ ":") line && String.length line > String.length label + 1) labels
        |> List.head


isWhole : String -> Bool
isWhole prompt =
    let
        found =
            List.filterMap labelOf (String.lines prompt)
    in
    List.member "Subject" found && List.length found >= 4


request : Job -> Request
request job =
    { model = modelId
    , messages = conversation job
    , temperature = 0.35
    , topP = 0.9
    , maxTokens = 320
    , thinking = False
    }


conversation : Job -> List Turn
conversation job =
    { role = System, content = instructions job.style }
        :: List.concatMap
            (\shown ->
                [ { role = User, content = shown.idea }
                , { role = Assistant, content = shown.prompt }
                ]
            )
            (examples job.style)
        ++ [ { role = User, content = job.idea } ]


instructions : Style -> String
instructions style =
    String.join "\n"
        [ "You write prompts for the FLUX.2 image model in the style \"" ++ styleName style ++ "\": " ++ look style ++ "."
        , ""
        , "Turn each idea into exactly these six lines, in this order:"
        , "Subject: the main subject from the idea and what it is doing, with concrete details"
        , "Style: the style words, plus a few that suit the idea"
        , "Setting: the place, the background and a few props"
        , "Lighting: the light source, its direction, the time of day and the colors"
        , "Camera: the lens, viewpoint, framing and composition"
        , "Mood: two or three words"
        , ""
        , "Rules:"
        , "- Keep every detail of the idea, add new details that fit it, and take nothing from earlier answers but their style words."
        , "- Describe only what is seen: never write \"no\", \"not\" or \"without\". Instead of \"no people\", write \"empty\"."
        , "- Words that must be written in the picture go in double quotes."
        , "- Short comma-separated phrases, concrete nouns and adjectives."
        , "- English, 60 to 100 words, the six lines and nothing else."
        ]


look : Style -> String
look style =
    case style of
        Photo ->
            "a realistic photograph: name a real camera body, a lens focal length and an aperture, and natural or practical light"

        Cinematic ->
            "a still from a feature film: anamorphic widescreen framing, motivated dramatic light, a color grade"

        AnalogFilm ->
            "a picture taken on analog film: name a film stock, visible grain, organic colors, a vintage camera"

        Anime ->
            "a hand-drawn anime frame: clean line art, cel shading, saturated colors, painted backgrounds"

        Watercolor ->
            "a watercolor painting: soft washes, wet edges, visible paper texture, white paper left as highlights"

        OilPainting ->
            "a classical oil painting on canvas: visible brushstrokes, rich pigments, painterly light"

        Render ->
            "a polished 3D render: stylized shapes, soft global illumination, physically based materials"

        PixelArt ->
            "retro pixel art: a visible pixel grid, a limited color palette, crisp hard edges"

        Comic ->
            "a comic book panel: bold ink outlines, flat colors, halftone dots, dynamic poses, speech balloons with quoted text when the idea has words"

        ProductShot ->
            "a commercial studio product photograph: a clean backdrop, controlled softbox lighting, crisp detail"


type alias Example =
    { idea : String
    , prompt : String
    }


examples : Style -> List Example
examples style =
    case style of
        Photo ->
            [ example "old man fishing at dawn"
                [ "an old fisherman with a grey beard and a weathered face casting a line from a wooden rowboat, his hands gripping the rod"
                , "documentary photograph, natural colors, fine detail in skin and fabric"
                , "a still mountain lake, pine forest on the far shore, thin mist over the water"
                , "first light of dawn, soft warm sun low behind the trees, gentle rim light on his face"
                , "shot on Fujifilm X-T5 with a 35mm f/1.4 lens, eye level, shallow depth of field"
                , "quiet, patient, peaceful"
                ]
            , example "deer in a forest"
                [ "a young red deer stag standing alert in a forest clearing, velvet antlers, breath steaming in the cold air"
                , "realistic wildlife photograph, natural colors, fine detail in the fur"
                , "an autumn beech forest, fallen copper leaves, mossy logs, morning mist between the trunks"
                , "low morning sun behind the trees, golden rim light on the fur, cool blue shadows"
                , "shot on Nikon Z8 with a 400mm f/2.8 lens, eye level, background softly blurred"
                , "wild, calm, majestic"
                ]
            ]

        Cinematic ->
            [ example "astronaut walking in the desert"
                [ "a lone astronaut in a scuffed white spacesuit walking across red sand dunes, visor reflecting the horizon"
                , "cinematic film still, teal and orange color grade, subtle film grain"
                , "an endless desert under a hazy sky, a crashed landing module half buried behind him"
                , "hard late-afternoon sun from the side, long shadows, warm dust glowing in the air"
                , "anamorphic 40mm lens, wide shot from low angle, 2.39:1 framing, shallow depth of field"
                , "lonely, epic, determined"
                ]
            , example "a woman in a red dress at a train station"
                [ "a woman in a flowing red dress standing alone on a crowded platform, holding a small suitcase, looking back over her shoulder"
                , "cinematic film still, color graded, subtle film grain, muted greens with a vivid red accent"
                , "a grand old train station, steam from a departing train, travelers in dark coats rushing past"
                , "shafts of morning light through tall arched windows, haze in the air, strong contrast"
                , "spherical 85mm lens, medium shot at eye level, 2.39:1 framing, background softly blurred"
                , "longing, romantic, tense"
                ]
            ]

        AnalogFilm ->
            [ example "girl on a bike in the city"
                [ "a young woman in a yellow raincoat riding a vintage bicycle, a paper bag of flowers in the basket"
                , "analog 35mm photograph, shot on Kodak Portra 400, natural grain, soft organic colors"
                , "a narrow cobblestone street in Paris, wet pavement, cafe tables along the sidewalk"
                , "overcast daylight, soft even light, gentle reflections on the wet stones"
                , "Leica M6 with a 50mm lens, slight motion blur on the wheels, medium shot"
                , "carefree, nostalgic, fresh"
                ]
            , example "old car parked by a diner"
                [ "a mint green 1950s convertible parked in front of a chrome roadside diner, a neon sign reading \"OPEN\" in the window"
                , "analog film photograph, natural grain, organic colors, slightly faded highlights"
                , "an empty desert highway, telephone poles, distant mesas and a dusty parking lot"
                , "low golden sunset light from the left, long shadows, warm glow on the chrome"
                , "Canon AE-1 with a 35mm lens, Kodak Gold 200, eye level, car on the right third"
                , "nostalgic, quiet, warm"
                ]
            ]

        Anime ->
            [ example "girl waiting at a train station in summer"
                [ "a schoolgirl with short black hair and a white sailor uniform waiting on a train platform, holding a bottle of soda"
                , "anime film still, clean line art, cel shading, saturated colors, detailed painted background"
                , "a small countryside station, rice fields and green hills behind, towering cumulus clouds"
                , "bright midsummer afternoon sun, crisp shadows, sparkling highlights on the bottle"
                , "wide shot from the end of the platform, rails leading to the horizon"
                , "nostalgic, warm, slightly wistful"
                ]
            , example "boy and his robot in a junkyard"
                [ "a boy in an oversized orange jacket sitting on a rusty pipe beside a tall friendly robot, both sharing a sandwich"
                , "anime film still, clean line art, cel shading, saturated colors, detailed painted background"
                , "a sprawling junkyard of old machines and stacked cars, wildflowers growing through the scrap, a city skyline far away"
                , "warm late-afternoon sun, crisp cel-shaded shadows, soft glow on the robot's eye lens"
                , "medium wide shot at a slight low angle, both characters on the left, sky filling the right"
                , "friendly, hopeful, gentle"
                ]
            ]

        Watercolor ->
            [ example "cottage by a river"
                [ "a small stone cottage with a red door beside a winding river, smoke rising from the chimney"
                , "loose watercolor painting, soft washes, wet blooming edges, visible cold-press paper texture"
                , "green meadow with wildflowers, willow trees leaning over the water, distant hills"
                , "soft morning light, pale sky left as white paper, gentle blue shadows"
                , "wide landscape view at eye level, cottage on the left third"
                , "calm, cozy, gentle"
                ]
            , example "a hummingbird"
                [ "a ruby-throated hummingbird hovering beside a cluster of pink fuchsia blossoms, wings caught mid-beat"
                , "loose watercolor painting, soft washes, wet blooming edges, visible paper texture, delicate ink details"
                , "a garden in summer, leaves suggested with green and yellow washes, background left mostly white"
                , "soft daylight, white paper left for highlights on the feathers, pale violet shadows"
                , "close view from the side, bird slightly left of center, generous white space around it"
                , "light, lively, delicate"
                ]
            ]

        OilPainting ->
            [ example "a cat sleeping on a chair"
                [ "a ginger cat curled up asleep on a velvet armchair, one paw over its nose"
                , "classical oil painting on canvas, visible impasto brushstrokes, rich warm pigments"
                , "a quiet study with bookshelves, a lace curtain and a teacup on a side table"
                , "warm afternoon window light from the left, deep chiaroscuro shadows"
                , "close view at chair height, cat at the center of the canvas"
                , "serene, intimate, warm"
                ]
            , example "fishing boats in a harbor"
                [ "three wooden fishing boats with faded blue and red hulls moored together, nets and ropes piled on their decks"
                , "classical oil painting on canvas, visible brushstrokes, rich pigments, impressionist touches"
                , "a small stone harbor, whitewashed houses with orange roofs on the hillside, gulls over the water"
                , "warm evening light from the west, glowing reflections on the rippled water, deep blue shadows"
                , "wide view from the harbor wall, boats in the foreground, houses rising behind"
                , "peaceful, golden, timeless"
                ]
            ]

        Render ->
            [ example "robot watering plants"
                [ "a small round robot with big glowing eyes watering a row of potted succulents with a tiny copper can"
                , "stylized 3D render, soft rounded shapes, physically based materials, subsurface scattering on the leaves"
                , "a sunny greenhouse with glass walls, wooden shelves and terracotta pots"
                , "soft global illumination, warm sunlight through the glass, gentle ambient occlusion"
                , "50mm lens at the robot's eye level, shallow depth of field, robot centered"
                , "cheerful, cute, cozy"
                ]
            , example "a snail with a mushroom house"
                [ "a cheerful snail carrying a tiny red-capped mushroom house on its back, with a round door and glowing windows"
                , "stylized 3D render, soft rounded shapes, physically based materials, glossy shell, velvety moss"
                , "a mossy forest floor, dewdrops on clover leaves, tall blurred ferns behind"
                , "soft global illumination, warm light from the windows, cool morning light, gentle ambient occlusion"
                , "85mm macro lens at ground level, shallow depth of field, snail centered"
                , "whimsical, cute, calm"
                ]
            ]

        PixelArt ->
            [ example "knight in front of a castle"
                [ "a small knight in silver armor with a red cape and a raised sword, standing on a stone bridge"
                , "16-bit pixel art, visible pixel grid, limited palette of 32 colors, crisp hard edges, retro video game look"
                , "a tall castle with blue banners behind the bridge, moat below, rolling green hills"
                , "flat bright daylight, simple two-tone shading, dithered sky gradient"
                , "side view like a platform game, knight in the foreground on the left"
                , "heroic, playful, adventurous"
                ]
            , example "spaceship battle"
                [ "a small white starfighter firing blue laser bolts at a huge purple alien mothership, explosions flashing along its hull"
                , "16-bit pixel art, visible pixel grid, limited color palette, crisp hard edges, retro video game look"
                , "deep space with a starfield, a ringed planet in the background, debris drifting by"
                , "simple flat light with two-tone shading, bright pixel glow around the lasers and explosions"
                , "side-scrolling shooter view, starfighter on the left, mothership filling the right"
                , "intense, exciting, retro"
                ]
            ]

        Comic ->
            [ example "superhero dog saying let's go"
                [ "a corgi in a red cape and blue mask leaping off a rooftop, a speech balloon reading \"LET'S GO!\""
                , "classic comic book panel, bold black ink outlines, flat bright colors, halftone dot shading"
                , "a city skyline at dusk, water towers and neon signs below"
                , "dramatic orange sunset behind him, strong rim light, deep ink shadows"
                , "low angle, dynamic diagonal composition, motion lines behind the dog"
                , "bold, funny, energetic"
                ]
            , example "scientist whose experiment exploded"
                [ "a startled scientist with singed hair and a soot-covered lab coat holding a smoking flask, a caption box reading \"OOPS...\""
                , "comic book panel, bold black ink outlines, flat bright colors, halftone dot shading, exaggerated expressions"
                , "a cluttered laboratory with bubbling beakers, sparking machines and papers flying through the air"
                , "bright green glow from the flask, dramatic light with deep ink shadows"
                , "close-up from a slight low angle, burst lines radiating from the flask"
                , "funny, chaotic, surprised"
                ]
            ]

        ProductShot ->
            [ example "perfume bottle"
                [ "a faceted glass perfume bottle with amber liquid and a gold cap, standing on a block of white marble"
                , "commercial product photograph, crisp detail, clean reflections, glossy finish"
                , "seamless pale beige backdrop, a few dried rose petals scattered beside the marble"
                , "three-point softbox setup, soft key light from the left, bright rim light outlining the glass"
                , "shot on Hasselblad X2D with an 80mm lens at f/8, eye level, centered composition"
                , "elegant, luxurious, minimal"
                ]
            , example "wireless headphones"
                [ "a pair of matte black wireless over-ear headphones with brushed aluminum hinges and soft leather cushions, resting on a small stone plinth"
                , "commercial product photograph, crisp detail, clean reflections, premium minimal look"
                , "seamless dark grey backdrop, a thin line of warm light across the floor"
                , "studio softbox from above, sharp rim light tracing the headband, subtle gradient on the backdrop"
                , "shot on Phase One with a 120mm macro lens at f/11, three-quarter view, centered composition"
                , "sleek, premium, calm"
                ]
            ]


example : String -> List String -> Example
example idea lines =
    { idea = idea
    , prompt =
        List.map2 (\label line -> label ++ ": " ++ line) labels lines
            |> String.join "\n"
    }


needsWebGpu : Msg -> Bool
needsWebGpu msg =
    case msg of
        StyleChosen _ _ ->
            True

        StopClicked ->
            False

        UndoClicked ->
            False

        RemoveClicked ->
            False

        Happened _ ->
            False
