module Plain exposing (frame, lead)

import ExploreView
import Html exposing (Html, a, div, p, span, text)
import Html.Attributes exposing (attribute, class, href)
import InteropDefinitions as Interop exposing (Theme)
import Logo
import Path
import Phosphor exposing (IconWeight(..))
import Svg.Attributes
import UI.Tabs as Tabs


frame : { theme : Theme, onTheme : Theme -> msg } -> List (Html msg) -> Html msg
frame { theme, onTheme } children =
    div [ class "grid min-h-svh bg-background text-foreground lg:grid-cols-2" ]
        [ div [ class "flex flex-col p-6 md:p-10" ]
            [ Html.header [ class "flex h-9 items-center" ]
                [ a
                    [ href (Path.toString Path.Explore)
                    , class "flex h-8 items-center rounded-lg ring-ring outline-hidden focus-visible:ring-2"
                    ]
                    [ Logo.wordmark [ Svg.Attributes.class "h-4 w-auto" ]
                    , span [ class "sr-only" ] [ text "pixo" ]
                    ]
                ]
            , Html.main_ [ class "flex flex-1 items-center justify-center py-10" ]
                [ div [ class "flex w-full max-w-sm flex-col gap-8" ] children ]
            , Html.footer [ class "flex h-9 items-center" ] [ viewThemeTabs theme onTheme ]
            ]
        , div [ class "hidden p-4 ps-0 lg:block", attribute "aria-hidden" "true" ]
            [ div [ class "relative h-full overflow-hidden mask-b-from-55%" ]
                [ div [ class "absolute inset-x-0 top-0 columns-3 gap-4" ] ExploreView.mosaic ]
            ]
        ]


lead : String -> String -> Html msg
lead heading body =
    div [ class "flex flex-col gap-2" ]
        [ Html.h1 [ class "text-2xl font-semibold tracking-tight" ] [ text heading ]
        , p [ class "text-sm text-pretty text-muted-foreground" ] [ text body ]
        ]


viewThemeTabs : Theme -> (Theme -> msg) -> Html msg
viewThemeTabs theme onTheme =
    Tabs.new
        |> Tabs.id "theme"
        |> Tabs.onSelect onTheme
        |> Tabs.value theme
        |> Tabs.tabs
            [ themeTab Interop.Light Phosphor.sun "Light"
            , themeTab Interop.Dark Phosphor.moon "Dark"
            , themeTab Interop.System Phosphor.monitor "System"
            ]
        |> Tabs.toHtml [ attribute "aria-label" "Theme" ]


themeTab : Theme -> Phosphor.Icon -> String -> Tabs.Tab Theme msg
themeTab theme icon label =
    Tabs.tab theme
        [ icon Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
        , span [ class "sr-only" ] [ text label ]
        ]
        []
