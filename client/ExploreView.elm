module ExploreView exposing (Invitation(..), gallery, mosaic, view)

import Html exposing (Html, div, h2, img, p, span, text)
import Html.Attributes exposing (alt, attribute, class, height, href, id, src, style, width)
import Overlay
import Path
import UI.Button as Button


type Invitation
    = Guest
    | Member


gallery : String
gallery =
    "gallery"


view : Invitation -> Html msg
view invitation =
    div [ class "flex flex-1 flex-col" ]
        [ banner invitation
        , div [ id gallery, class "flex flex-col gap-6 p-4 pt-8" ]
            [ div [ class "flex flex-col gap-1" ]
                [ h2 [ class "text-2xl font-semibold tracking-tight" ] [ text "See what people are making" ]
                , p [ class "text-sm text-pretty text-muted-foreground" ] [ text "Every picture here was made in a browser." ]
                ]
            , div [ class "columns-2 gap-4 md:columns-3 xl:columns-4 2xl:columns-5", attribute "aria-hidden" "true" ]
                mosaic
            ]
        ]


mosaic : List (Html msg)
mosaic =
    List.map viewTile tiles


banner : Invitation -> Html msg
banner invitation =
    Html.section [ class "relative flex min-h-[calc(100svh-3rem)] flex-col justify-end text-white" ]
        [ div [ class "absolute inset-0 bg-neutral-900 mask-dissolve" ]
            [ img
                [ src (cover 1600)
                , attribute "srcset" (String.join ", " (List.map (\wide -> cover wide ++ " " ++ String.fromInt wide ++ "w") [ 800, 1600, 2400 ]))
                , attribute "sizes" "(orientation: portrait) 150vh, 100vw"
                , attribute "fetchpriority" "high"
                , alt ""
                , class "absolute inset-0 size-full object-cover"
                ]
                []
            , div [ class "absolute inset-0 bg-linear-to-t from-black/30 via-black/45 to-transparent" ] []
            ]
        , div [ class "relative flex flex-col gap-6 p-6 pb-[29svh] md:p-10 md:pb-[29svh] lg:p-14 lg:pb-[29svh]" ]
            [ div [ class "flex flex-col gap-4" ]
                [ h2 [ class "text-4xl leading-none font-semibold tracking-tight text-balance sm:text-5xl lg:text-7xl" ]
                    [ text "Unplug the cloud." ]
                , p [ class "max-w-xl text-pretty text-white/80 md:text-lg" ]
                    [ span [ class "block" ] [ text "No tokens, no credits, no one watching." ]
                    , span [ class "block" ] [ text "Unlimited local & private AI image generation." ]
                    ]
                ]
            , div [ class "flex flex-wrap items-center gap-2" ]
                (case invitation of
                    Guest ->
                        [ primary "Create an account" (Path.toString Path.SignUp)
                        , secondary "Log in" (Path.toString Path.SignIn)
                        ]

                    Member ->
                        [ primary "Start creating" (Path.toString Path.Studio)
                        , secondary "Explore creations" ("#" ++ gallery)
                        ]
                )
            ]
        ]


primary : String -> String -> Html msg
primary label url =
    Button.new
        |> Button.text label
        |> Button.toAnchor [ href url, Overlay.solid, class "h-11! px-5! text-base!" ]


secondary : String -> String -> Html msg
secondary label url =
    Button.new
        |> Button.text label
        |> Button.link
        |> Button.toAnchor [ href url, Overlay.link, class "h-11! px-5! text-base!" ]


cover : Int -> String
cover wide =
    "https://images.unsplash.com/photo-1519681393784-d120267933ba?auto=format&fit=crop&q=70&w=" ++ String.fromInt wide


type alias Tile =
    { width : Int
    , height : Int
    , photo : String
    }


viewTile : Tile -> Html msg
viewTile tile =
    div
        [ class "mb-4 break-inside-avoid overflow-hidden rounded-xl bg-muted shadow-sm ring-1 ring-foreground/10"
        , style "aspect-ratio" (String.fromInt tile.width ++ " / " ++ String.fromInt tile.height)
        ]
        [ img
            [ src (source tile)
            , alt ""
            , width tile.width
            , height tile.height
            , attribute "loading" "lazy"
            , class "size-full object-cover"
            ]
            []
        ]


source : Tile -> String
source tile =
    "https://images.unsplash.com/photo-"
        ++ tile.photo
        ++ "?auto=format&fit=crop&q=60&w="
        ++ String.fromInt tile.width
        ++ "&h="
        ++ String.fromInt tile.height


tiles : List Tile
tiles =
    [ Tile 480 600 "1474511320723-9a56873867b5"
    , Tile 480 480 "1541701494587-cb58502866ab"
    , Tile 640 360 "1501785888041-af3ef285b470"
    , Tile 450 800 "1433086966358-54859d0ed716"
    , Tile 480 720 "1533738363-b7f9aef128ce"
    , Tile 480 480 "1437622368342-7a3d73a34c8f"
    , Tile 640 320 "1493246507139-91e8fad9978e"
    , Tile 480 600 "1444703686981-a3abbc4d4fe3"
    , Tile 640 360 "1500530855697-b586d89ba3ee"
    , Tile 480 720 "1447752875215-b2761acb3c5d"
    , Tile 480 480 "1517849845537-4d257902454a"
    , Tile 450 800 "1513836279014-a89f7a76ae86"
    , Tile 720 240 "1475924156734-496f6cac6ec1"
    , Tile 480 600 "1550684848-fac1c5b4e853"
    , Tile 480 480 "1567620905732-2d1ec7ab7445"
    , Tile 640 360 "1519681393784-d120267933ba"
    , Tile 480 720 "1500462918059-b1a0cb512f1d"
    , Tile 480 480 "1490750967868-88aa4486c946"
    , Tile 480 600 "1505142468610-359e7d316be0"
    , Tile 640 320 "1472214103451-9374bd1c798e"
    ]
