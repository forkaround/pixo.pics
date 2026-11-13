module Main exposing (Model, Msg(..), init, main, update, view)

import Api
import ApiClient
import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Navigation
import Data.Email as Email
import Data.Username as Username
import Effect exposing (Effect(..))
import Enrolment
import ExploreView
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (attribute, class, href)
import Html.Events exposing (onClick)
import InteropDefinitions as Interop exposing (Theme)
import InteropPorts
import Json.Decode as Decode
import Library
import LibraryView
import Logo
import Minidenticons
import ModelsView
import Page.About
import Page.Account
import Page.Passkey
import Page.SignIn
import Page.SignUp
import Page.Verify
import Path
import Phosphor exposing (IconWeight(..))
import Pipeline
import Plain
import Session exposing (Session(..))
import StudioView
import Stylist
import SupportView
import Svg.Attributes
import Task
import Time
import UI.Avatar as Avatar
import UI.Button as Button
import UI.Card as Card
import UI.DropdownMenu as DropdownMenu
import UI.Sidebar as Sidebar
import UI.Skeleton as Skeleton
import UI.Tabs as Tabs
import Url exposing (Url)


main : Program Decode.Value ( Navigation.Key, Model ) Msg
main =
    Browser.application
        { init =
            \flags url key ->
                init flags url
                    |> Tuple.mapBoth (Tuple.pair key) (perform key)
        , update =
            \msg ( key, model ) ->
                update msg model
                    |> Tuple.mapBoth (Tuple.pair key) (perform key)
        , view = Tuple.second >> view
        , subscriptions = Tuple.second >> subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { theme : Theme
    , systemDark : Bool
    , session : Session
    , pipeline : Pipeline.Model
    , stylist : Stylist.Model
    , library : Library.Model
    , sidebar : Sidebar.State
    , sidebarProgress : Float
    , generations : Int
    , turns : Int
    , page : Page
    }


type Page
    = Explore
    | Studio
    | Library
    | Settings Section
    | SignUp Page.SignUp.Model
    | SignIn Page.SignIn.Model
    | Verify Page.Verify.Model
    | Passkey Page.Passkey.Model
    | NotFound


type Section
    = About Page.About.Model
    | Models
    | Account Page.Account.Model


init : Decode.Value -> Url -> ( Model, Effect Msg )
init flags url =
    let
        { theme, systemDark, entropy, models, stylist, canShare, sidebarOpen, webGpu } =
            InteropPorts.decodeFlags flags
                |> Result.withDefault { theme = Nothing, systemDark = False, entropy = 0, models = Nothing, stylist = Nothing, canShare = False, sidebarOpen = Nothing, webGpu = True }

        sidebar =
            { open = Maybe.withDefault True sidebarOpen, openMobile = False }
    in
    open url
        { theme = Maybe.withDefault Interop.System theme
        , systemDark = systemDark
        , session = Unknown
        , pipeline = Pipeline.init entropy webGpu (Maybe.withDefault [] (Maybe.map Pipeline.remembered models))
        , stylist = Stylist.init stylist
        , library = Library.init canShare
        , sidebar = sidebar
        , sidebarProgress = sidebarTarget sidebar
        , generations = 0
        , turns = 0
        , page = NotFound
        }
        |> Tuple.mapSecond (\effect -> Batch [ Request (ApiClient.request Api.session () GotSession), effect ])


open : Url -> Model -> ( Model, Effect Msg )
open url opened =
    let
        model =
            { opened | sidebar = { open = opened.sidebar.open, openMobile = False } }
    in
    case Path.parse url of
        Path.Explore ->
            ( { model | page = Explore }, NoEffect )

        Path.Studio ->
            runPipeline { model | page = Studio } (Pipeline.enter model.pipeline)

        Path.Library ->
            runLibrary { model | page = Library } (Library.enter model.library)

        Path.About ->
            Page.About.load
                |> Tuple.mapBoth (\page -> { model | page = Settings (About page) }) (Effect.map GotAbout)

        Path.Models ->
            runPipeline { model | page = Settings Models } (Pipeline.enter model.pipeline)

        Path.SignUp ->
            ( { model | page = SignUp Page.SignUp.init }, NoEffect )

        Path.SignIn ->
            ( { model | page = SignIn Page.SignIn.init }, NoEffect )

        Path.Account ->
            if model.session == SignedOut then
                ( { model | page = Settings (Account Page.Account.init) }, PushUrl (Path.toString Path.SignIn) )

            else
                Page.Account.load Page.Account.init
                    |> Tuple.mapBoth (\page -> { model | page = Settings (Account page) }) (Effect.map GotAccount)

        Path.Verify link ->
            Page.Verify.init link
                |> Tuple.mapBoth (\page -> { model | page = Verify page }) (Effect.map GotVerify)

        Path.Passkey ->
            ( { model | page = Passkey Page.Passkey.init }
            , if model.session == SignedOut then
                PushUrl (Path.toString Path.SignIn)

              else
                NoEffect
            )

        Path.NotFound ->
            ( { model | page = NotFound }, NoEffect )


runPipeline : Model -> ( Pipeline.Model, Maybe Pipeline.Command ) -> ( Model, Effect Msg )
runPipeline model ( pipeline, command ) =
    ( { model
        | pipeline = pipeline
        , generations =
            if isGenerating pipeline.device && not (isGenerating model.pipeline.device) then
                model.generations + 1

            else
                model.generations
      }
    , Batch
        (List.filterMap identity
            [ Maybe.map (Interop.RunPipeline >> SendToJs) command
            , if pipeline.downloaded == model.pipeline.downloaded then
                Nothing

              else
                Just
                    (SendToJs
                        (Interop.WriteToLocalStorage
                            { key = "models"
                            , value = Pipeline.remember pipeline.downloaded
                            }
                        )
                    )
            ]
        )
    )


runStylist : Model -> ( Stylist.Model, Maybe Stylist.Command, Maybe String ) -> ( Model, Effect Msg )
runStylist model ( stylist, command, prompt ) =
    let
        ( written, effect ) =
            case prompt of
                Just text ->
                    runPipeline model (Pipeline.update (Pipeline.PromptChanged text) model.pipeline)

                Nothing ->
                    ( model, NoEffect )
    in
    ( { written | stylist = stylist }
    , Batch
        (List.filterMap identity
            [ Just effect
            , Maybe.map (Interop.RunStylist >> SendToJs) command
            , if stylist.downloaded == model.stylist.downloaded then
                Nothing

              else
                Just
                    (SendToJs
                        (Interop.WriteToLocalStorage
                            { key = "stylist"
                            , value =
                                if stylist.downloaded then
                                    Stylist.modelId

                                else
                                    ""
                            }
                        )
                    )
            ]
        )
    )


runLibrary : Model -> ( Library.Model, List Library.Command ) -> ( Model, Effect Msg )
runLibrary model ( library, commands ) =
    ( { model | library = library }
    , Batch (List.map (Interop.RunLibrary >> SendToJs) commands)
    )


isDark : Model -> Bool
isDark model =
    case model.theme of
        Interop.Light ->
            False

        Interop.Dark ->
            True

        Interop.System ->
            model.systemDark



-- UPDATE


type Msg
    = GotInterop (Result Decode.Error Interop.ToElm)
    | ThemeSelected Theme
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | GotSignUp Page.SignUp.Msg
    | GotVerify Page.Verify.Msg
    | GotSession (Result ApiClient.Error Api.Account)
    | SessionRetried
    | GotSignIn Page.SignIn.Msg
    | SignOutClicked
    | GotSignOut (Result ApiClient.Error ())
    | GotPasskey Page.Passkey.Msg
    | GotAbout Page.About.Msg
    | GotAccount Page.Account.Msg
    | PathSelected Path.Path
    | GotPipeline Pipeline.Msg
    | GotLibrary Library.Msg
    | GotStylist Stylist.Msg
    | SidebarChanged Sidebar.State
    | SidebarAnimated Float
    | HourglassTurned
    | Scrolled


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GotInterop InteropPorts.toElm
        , if model.sidebarProgress == sidebarTarget model.sidebar then
            Sub.none

          else
            Browser.Events.onAnimationFrameDelta SidebarAnimated
        , if isGenerating model.pipeline.device then
            Time.every 4000 (\_ -> HourglassTurned)

          else
            Sub.none
        ]


sidebarTarget : Sidebar.State -> Float
sidebarTarget sidebar =
    if sidebar.open then
        0

    else
        1


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotInterop (Ok (Interop.SystemThemeChanged systemDark)) ->
            ( { model | systemDark = systemDark }, NoEffect )

        GotInterop (Ok (Interop.PasskeyCreated attestation)) ->
            enrol (Enrolment.created attestation) model

        GotInterop (Ok (Interop.PasskeyUsed assertion)) ->
            update (GotSignIn (Page.SignIn.passkeyUsed assertion)) model

        GotInterop (Ok (Interop.PasskeyFailed name)) ->
            case model.page of
                SignIn _ ->
                    update (GotSignIn (Page.SignIn.passkeyFailed name)) model

                _ ->
                    enrol (Enrolment.failed name) model

        GotInterop (Ok (Interop.PipelineEvent event)) ->
            runPipeline model (Pipeline.update (Pipeline.Happened event) model.pipeline)

        GotInterop (Ok (Interop.LibraryEvent event)) ->
            runLibrary model (Library.update (Library.Happened event) model.library)

        GotInterop (Ok (Interop.StylistEvent event)) ->
            runStylist model (Stylist.update (Stylist.Happened event) model.stylist)

        GotInterop (Err _) ->
            ( model, NoEffect )

        GotPipeline pipelineMsg ->
            runPipeline model (Pipeline.update pipelineMsg model.pipeline)

        GotLibrary libraryMsg ->
            runLibrary model (Library.update libraryMsg model.library)

        GotStylist stylistMsg ->
            if Stylist.needsWebGpu stylistMsg && not (Pipeline.isSupported model.pipeline.device) then
                ( model, NoEffect )

            else
                runStylist model (Stylist.update stylistMsg model.stylist)

        SidebarChanged sidebar ->
            ( { model | sidebar = sidebar }
            , if sidebar.open == model.sidebar.open then
                NoEffect

              else
                SendToJs
                    (Interop.WriteToLocalStorage
                        { key = "sidebar"
                        , value =
                            if sidebar.open then
                                "expanded"

                            else
                                "collapsed"
                        }
                    )
            )

        SidebarAnimated delta ->
            ( { model
                | sidebarProgress =
                    if model.sidebar.open then
                        max 0 (model.sidebarProgress - max 0 delta / 200)

                    else
                        min 1 (model.sidebarProgress + max 0 delta / 200)
              }
            , NoEffect
            )

        HourglassTurned ->
            ( { model | turns = model.turns + 1 }, NoEffect )

        PathSelected path ->
            ( model, PushUrl (Path.toString path) )

        UrlRequested (Browser.Internal url) ->
            if model.page == Explore && Path.parse url == Path.Explore && url.fragment == Just ExploreView.gallery then
                ( model, ScrollTo ExploreView.gallery )

            else
                ( model, PushUrl (Url.toString url) )

        Scrolled ->
            ( model, NoEffect )

        UrlRequested (Browser.External url) ->
            ( model, Load url )

        UrlChanged url ->
            open url model

        GotSignUp pageMsg ->
            case model.page of
                SignUp page ->
                    Page.SignUp.update pageMsg page
                        |> Tuple.mapBoth (\next -> { model | page = SignUp next }) (Effect.map GotSignUp)

                _ ->
                    ( model, NoEffect )

        GotVerify pageMsg ->
            let
                believed =
                    case Page.Verify.outcome pageMsg of
                        Page.Verify.SignedIn account ->
                            { model | session = SignedIn account }

                        Page.Verify.Unchanged ->
                            model
            in
            case model.page of
                Verify page ->
                    Page.Verify.update pageMsg page
                        |> Tuple.mapBoth (\next -> { believed | page = Verify next }) (Effect.map GotVerify)

                _ ->
                    ( believed, NoEffect )

        GotSession (Ok account) ->
            ( { model | session = SignedIn account }, NoEffect )

        GotSession (Err error) ->
            case ( model.session, error ) of
                ( SignedIn _, _ ) ->
                    ( model, NoEffect )

                ( _, ApiClient.Unauthorized _ ) ->
                    ( { model | session = SignedOut }
                    , case model.page of
                        Settings (Account _) ->
                            PushUrl (Path.toString Path.SignIn)

                        Passkey _ ->
                            PushUrl (Path.toString Path.SignIn)

                        _ ->
                            NoEffect
                    )

                ( _, ApiClient.Failed message ) ->
                    ( { model | session = Unavailable message }, NoEffect )

        SessionRetried ->
            case model.page of
                Settings (Account account) ->
                    let
                        ( page, effect ) =
                            Page.Account.load account
                    in
                    ( { model | session = Unknown, page = Settings (Account page) }
                    , Batch
                        [ Request (ApiClient.request Api.session () GotSession)
                        , Effect.map GotAccount effect
                        ]
                    )

                _ ->
                    ( { model | session = Unknown }, Request (ApiClient.request Api.session () GotSession) )

        GotSignIn pageMsg ->
            let
                believed =
                    case Page.SignIn.outcome pageMsg of
                        Page.SignIn.SignedIn account ->
                            { model | session = SignedIn account }

                        Page.SignIn.Unchanged ->
                            model
            in
            case model.page of
                SignIn page ->
                    Page.SignIn.update pageMsg page
                        |> Tuple.mapBoth (\next -> { believed | page = SignIn next }) (Effect.map GotSignIn)

                _ ->
                    ( believed, NoEffect )

        SignOutClicked ->
            ( model, Request (ApiClient.request Api.signOut () GotSignOut) )

        GotSignOut (Ok ()) ->
            ( { model | session = SignedOut }, PushUrl (Path.toString Path.Explore) )

        GotSignOut (Err _) ->
            ( model, NoEffect )

        GotPasskey pageMsg ->
            case model.page of
                Passkey page ->
                    Page.Passkey.update pageMsg page
                        |> Tuple.mapBoth (\next -> { model | page = Passkey next }) (Effect.map GotPasskey)

                _ ->
                    ( model, NoEffect )

        GotAbout pageMsg ->
            case model.page of
                Settings (About page) ->
                    Page.About.update pageMsg page
                        |> Tuple.mapBoth (\next -> { model | page = Settings (About next) }) (Effect.map GotAbout)

                _ ->
                    ( model, NoEffect )

        GotAccount pageMsg ->
            let
                believed =
                    case Page.Account.outcome pageMsg of
                        Page.Account.SignedOut ->
                            { model | session = SignedOut }

                        Page.Account.Unchanged ->
                            model
            in
            case model.page of
                Settings (Account page) ->
                    Page.Account.update model.session pageMsg page
                        |> Tuple.mapBoth (\next -> { believed | page = Settings (Account next) }) (Effect.map GotAccount)

                _ ->
                    ( believed, NoEffect )

        ThemeSelected theme ->
            ( { model | theme = theme }
            , SendToJs
                (Interop.WriteToLocalStorage
                    { key = "theme"
                    , value = Interop.themeToString theme
                    }
                )
            )


enrol : Enrolment.Msg -> Model -> ( Model, Effect Msg )
enrol msg model =
    case model.page of
        Passkey _ ->
            update (GotPasskey (Page.Passkey.enrolment msg)) model

        Settings (Account _) ->
            update (GotAccount (Page.Account.enrolment msg)) model

        _ ->
            ( model, NoEffect )



-- EFFECTS


perform : Navigation.Key -> Effect Msg -> Cmd Msg
perform key effect =
    case effect of
        NoEffect ->
            Cmd.none

        Batch effects ->
            Cmd.batch (List.map (perform key) effects)

        Request request ->
            ApiClient.send request

        SendToJs value ->
            InteropPorts.fromElm value

        PushUrl url ->
            Navigation.pushUrl key url

        Load url ->
            Navigation.load url

        ReadTime toMsg ->
            Task.perform toMsg Time.now

        ScrollTo target ->
            Task.map2 (\element bar -> element.element.y - bar.element.height)
                (Dom.getElement target)
                (Dom.getElement "bar")
                |> Task.andThen (Dom.setViewport 0)
                |> Task.attempt (always Scrolled)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = title model.page
    , body =
        [ div
            [ class
                (if isDark model then
                    "dark"

                 else
                    ""
                )
            , attribute "style" ("--sidebar-progress: " ++ String.fromFloat model.sidebarProgress)
            ]
            [ viewBody model ]
        ]
    }


title : Page -> String
title page =
    case page of
        Explore ->
            "Explore · pixo"

        Studio ->
            "Studio · pixo"

        Library ->
            "Library · pixo"

        Settings (About _) ->
            "About · pixo"

        Settings Models ->
            "Models · pixo"

        Settings (Account _) ->
            "Account · pixo"

        SignUp _ ->
            "Sign up · pixo"

        SignIn _ ->
            "Log in · pixo"

        Verify _ ->
            "Verify · pixo"

        Passkey _ ->
            "Create a passkey · pixo"

        NotFound ->
            "Not found · pixo"


viewBody : Model -> Html Msg
viewBody model =
    case model.page of
        Studio ->
            viewFrame model
                "Studio"
                []
                [ viewAside "border-b md:w-80 md:border-e md:border-b-0" [ StudioView.controls { pipeline = GotPipeline, stylist = GotStylist } model.stylist model.pipeline ]
                , viewContent
                    [ StudioView.canvas { pipeline = GotPipeline, library = GotLibrary } model.library model.pipeline
                    , StudioView.lightbox { pipeline = GotPipeline, library = GotLibrary } model.library model.pipeline
                    ]
                ]

        Library ->
            viewFrame model
                "Library"
                [ Html.map GotLibrary (LibraryView.search model.library) ]
                [ viewContent [ Html.map GotLibrary (LibraryView.view model.library) ] ]

        Explore ->
            viewFrame model "Explore" [] [ viewContent [ ExploreView.view (invitation model.session) ] ]

        Settings section ->
            viewFrame model
                "Settings"
                []
                [ viewAside "md:w-56 md:pt-6"
                    [ Sidebar.group []
                        [ Sidebar.menu [ class "max-md:flex-row max-md:flex-wrap" ]
                            (viewDestination (isAbout model.page) Path.About Phosphor.info "About"
                                :: viewDestination (section == Models) Path.Models Phosphor.cube "Models"
                                :: (if isAccount model.page || isSignedIn model.session then
                                        [ viewDestination (isAccount model.page) Path.Account Phosphor.userCircle "Account" ]

                                    else
                                        []
                                   )
                            )
                        ]
                    ]
                , viewContent
                    [ case section of
                        About about ->
                            viewSettingsPage "About"
                                (text "What stays on this device, and the server this app talks to.")
                                (Page.About.view about)

                        Models ->
                            viewSettingsPage "Models"
                                (SupportView.layers []
                                    model.pipeline.device
                                    { checking =
                                        div [ attribute "aria-hidden" "true" ]
                                            [ Skeleton.view [ class "my-1 h-3.5 w-80 max-w-full" ] ]
                                    , supported = text "Manage the models that run in your browser. Your prompts stay on this device."
                                    , unsupported = text (SupportView.unsupportedTitle ++ ". " ++ SupportView.unsupportedReason ++ ".")
                                    }
                                )
                                [ ModelsView.view { pipeline = GotPipeline, stylist = GotStylist } model.stylist model.pipeline ]

                        Account account ->
                            viewSettingsPage "Account"
                                (case model.session of
                                    SignedIn identity ->
                                        div [ class "flex flex-wrap items-center gap-x-2" ]
                                            [ span [] [ text ("Signed in as " ++ Username.toString identity.username ++ " (" ++ Email.toString identity.email ++ ")") ]
                                            , Button.new
                                                |> Button.text "Sign out"
                                                |> Button.link
                                                |> Button.small
                                                |> Button.toHtml [ onClick SignOutClicked, class "px-0!" ]
                                            ]

                                    Unknown ->
                                        div [ class "flex h-7 items-center", attribute "aria-hidden" "true" ]
                                            [ Skeleton.view [ class "h-3.5 w-80 max-w-full" ] ]

                                    _ ->
                                        text "Manage your account and how you sign in."
                                )
                                (case model.session of
                                    Unavailable message ->
                                        [ notice "Can't check who is signed in" message [ retry ] ]

                                    _ ->
                                        List.map (Html.map GotAccount) (Page.Account.view model.session account)
                                )
                    ]
                ]

        SignUp signUp ->
            Plain.frame { theme = model.theme, onTheme = ThemeSelected } (viewAuthTabs Path.SignUp :: List.map (Html.map GotSignUp) (Page.SignUp.view signUp))

        SignIn signIn ->
            Plain.frame { theme = model.theme, onTheme = ThemeSelected } (viewAuthTabs Path.SignIn :: List.map (Html.map GotSignIn) (Page.SignIn.view signIn))

        Verify verification ->
            Plain.frame { theme = model.theme, onTheme = ThemeSelected } (List.map (Html.map GotVerify) (Page.Verify.view verification))

        Passkey page ->
            Plain.frame { theme = model.theme, onTheme = ThemeSelected } (List.map (Html.map GotPasskey) (Page.Passkey.view page))

        NotFound ->
            Plain.frame { theme = model.theme, onTheme = ThemeSelected }
                [ Plain.lead "Not found" "There is nothing at this address."
                , div [ class "flex flex-col gap-4" ]
                    [ Button.new
                        |> Button.text "Go home"
                        |> Button.toAnchor [ href (Path.toString Path.Explore), class "h-10" ]
                    , div [ class "flex min-h-10 gap-4" ] (notFoundOptions model.session)
                    ]
                ]


viewSettingsPage : String -> Html Msg -> List (Html Msg) -> Html Msg
viewSettingsPage heading description children =
    div [ class "mx-auto flex w-full max-w-3xl flex-col gap-8 p-4 md:p-8" ]
        [ Html.header [ class "flex flex-col gap-2" ]
            [ Html.h2 [ class "text-2xl font-semibold tracking-tight" ] [ text heading ]
            , div [ class "text-sm leading-relaxed wrap-anywhere text-muted-foreground" ] [ description ]
            ]
        , div [ class "flex flex-col gap-6" ] children
        ]


viewFrame : Model -> String -> List (Html Msg) -> List (Html Msg) -> Html Msg
viewFrame model heading tools page =
    viewShell model
        [ viewBar model heading tools
        , div [ class "flex min-w-0 flex-1 flex-col md:flex-row" ] page
        ]


viewShell : Model -> List (Html Msg) -> Html Msg
viewShell model page =
    Sidebar.new
        |> Sidebar.state model.sidebar SidebarChanged
        |> Sidebar.collapsesToIcons
        |> Sidebar.width "15rem"
        |> Sidebar.header
            [ a
                [ href (Path.toString Path.Explore)
                , class "flex h-8 items-center rounded-lg px-2 text-sidebar-foreground ring-sidebar-ring outline-hidden focus-visible:ring-2"
                ]
                [ span [ class "sidebar-logo flex h-4 shrink-0" ]
                    [ Logo.sidebar model.sidebarProgress
                        { running = isGenerating model.pipeline.device
                        , run = model.generations
                        , turn = model.turns
                        }
                        [ Svg.Attributes.class "h-4 w-auto max-w-none shrink-0" ]
                    ]
                , span [ class "sr-only" ] [ text "pixo" ]
                ]
            ]
        |> Sidebar.content
            [ Sidebar.group []
                [ Sidebar.menu []
                    [ viewDestination (model.page == Explore) Path.Explore Phosphor.kanban "Explore"
                    , viewDestination (model.page == Studio) Path.Studio Phosphor.stack "Studio"
                    , viewDestination (model.page == Library) Path.Library Phosphor.slideshow "Library"
                    , viewDestination (isSettings model.page) Path.Models Phosphor.sliders "Settings"
                    ]
                ]
            ]
        |> Sidebar.footer
            [ Sidebar.menu [] [ viewSession model ] ]
        |> Sidebar.rail
        |> Sidebar.page [ class "min-w-0" ] page
        |> Sidebar.toHtml [ class "sidebar-morph bg-background text-foreground" ]


isGenerating : Pipeline.Device -> Bool
isGenerating device =
    case device of
        Pipeline.Supported _ (Pipeline.Generating _ _ (Pipeline.Denoising _)) ->
            True

        _ ->
            False


invitation : Session -> ExploreView.Invitation
invitation session =
    case session of
        SignedIn _ ->
            ExploreView.Member

        _ ->
            ExploreView.Guest


isAbout : Page -> Bool
isAbout page =
    case page of
        Settings (About _) ->
            True

        _ ->
            False


isAccount : Page -> Bool
isAccount page =
    case page of
        Settings (Account _) ->
            True

        _ ->
            False


isSignedIn : Session -> Bool
isSignedIn session =
    case session of
        SignedIn _ ->
            True

        _ ->
            False


isSettings : Page -> Bool
isSettings page =
    case page of
        Settings _ ->
            True

        _ ->
            False


viewDestination : Bool -> Path.Path -> Phosphor.Icon -> String -> Html Msg
viewDestination current path icon label =
    Sidebar.menuItem []
        [ Sidebar.menuLink
            (href (Path.toString path)
                :: Sidebar.tooltip label
                :: (if current then
                        [ Sidebar.active, attribute "aria-current" "page" ]

                    else
                        []
                   )
            )
            [ icon Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
            , span [] [ text label ]
            ]
        ]


viewSession : Model -> Html Msg
viewSession model =
    let
        icon shape =
            shape Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]

        guest detail actions =
            Sidebar.menuItem [ class "flex h-12 items-center" ]
                [ viewUserMenu model.theme
                    { avatar = [ icon Phosphor.user ]
                    , name = "Guest"
                    , detail = detail
                    , actions = actions
                    , leave = []
                    }
                ]
    in
    case model.session of
        Unknown ->
            Sidebar.menuItem [ attribute "aria-hidden" "true" ]
                [ div [ class "flex h-12 items-center gap-2 p-2 transition-[padding] md:group-data-[collapsible=icon]:p-0" ]
                    [ Skeleton.view [ class "size-8 shrink-0 rounded-full!" ]
                    , div [ class "flex flex-1 flex-col gap-1.5 md:group-data-[collapsible=icon]:hidden" ]
                        [ Skeleton.view [ class "h-3.5 w-20" ]
                        , Skeleton.view [ class "h-3 w-32" ]
                        ]
                    ]
                ]

        Unavailable _ ->
            guest "Can't check who is signed in"
                [ DropdownMenu.item [ onClick SessionRetried ] [ icon Phosphor.arrowClockwise, text "Try again" ] ]

        SignedOut ->
            guest "Not signed in"
                [ DropdownMenu.item [ onClick (PathSelected Path.SignIn) ] [ icon Phosphor.signIn, text "Log in" ]
                , DropdownMenu.item [ onClick (PathSelected Path.SignUp) ] [ icon Phosphor.userPlus, text "Create an account" ]
                ]

        SignedIn account ->
            Sidebar.menuItem [ class "flex h-12 items-center" ]
                [ viewUserMenu model.theme
                    { avatar = [ span [ class "size-full *:size-full!" ] [ Minidenticons.identicon 75 50 (Username.toString account.username) ] ]
                    , name = Username.toString account.username
                    , detail = Email.toString account.email
                    , actions =
                        [ DropdownMenu.item [ onClick (PathSelected Path.Account) ]
                            [ icon Phosphor.userCircle, text "Account settings" ]
                        ]
                    , leave =
                        [ DropdownMenu.item [ onClick SignOutClicked ] [ icon Phosphor.signOut, text "Sign out" ] ]
                    }
                ]


viewUserMenu :
    Theme
    ->
        { avatar : List (Html Msg)
        , name : String
        , detail : String
        , actions : List (DropdownMenu.Item Msg)
        , leave : List (DropdownMenu.Item Msg)
        }
    -> Html Msg
viewUserMenu theme user =
    let
        avatar =
            Avatar.new
                |> Avatar.fallback user.avatar
                |> Avatar.toHtml [ attribute "aria-hidden" "true" ]

        details =
            div [ class "grid flex-1 text-start text-sm leading-tight" ]
                [ span [ class "truncate font-medium text-sidebar-foreground" ] [ text user.name ]
                , span [ class "truncate text-xs" ] [ text user.detail ]
                ]

        themeItem choice shape label =
            DropdownMenu.radioItem (theme == choice)
                [ onClick (ThemeSelected choice) ]
                [ shape Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ], text label ]
    in
    DropdownMenu.new
        |> DropdownMenu.id "user"
        |> DropdownMenu.trigger (Sidebar.menuButtonAttributes ++ [ Sidebar.large, class "transition-[width,height,padding]! md:group-data-[collapsible=icon]:rounded-full! md:group-data-[collapsible=icon]:bg-transparent! md:group-data-[collapsible=icon]:ring-offset-2 md:group-data-[collapsible=icon]:ring-offset-sidebar md:group-data-[collapsible=icon]:hover:ring-2 md:group-data-[collapsible=icon]:hover:ring-sidebar-accent md:group-data-[collapsible=icon]:[&:has(+:popover-open)]:ring-2 md:group-data-[collapsible=icon]:[&:has(+:popover-open)]:ring-sidebar-accent" ])
            [ avatar
            , details
            , Phosphor.caretUpDown Bold
                |> Phosphor.withClass "ms-auto size-4"
                |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
            ]
        |> DropdownMenu.content
            ([ DropdownMenu.group user.actions
             , DropdownMenu.separator
             , DropdownMenu.group
                [ DropdownMenu.label [ text "Theme" ]
                , themeItem Interop.Light Phosphor.sun "Light"
                , themeItem Interop.Dark Phosphor.moon "Dark"
                , themeItem Interop.System Phosphor.monitor "System"
                ]
             ]
                ++ (if List.isEmpty user.leave then
                        []

                    else
                        [ DropdownMenu.separator, DropdownMenu.group user.leave ]
                   )
            )
        |> DropdownMenu.right
        |> DropdownMenu.alignEnd
        |> DropdownMenu.toHtml [ class "min-w-56" ]


viewAside : String -> List (Html Msg) -> Html Msg
viewAside wide children =
    Html.aside
        [ class "flex w-full shrink-0 flex-col bg-background md:sticky md:top-12 md:h-[calc(100svh-3rem)] md:overflow-y-auto"
        , class wide
        ]
        children


viewBar : Model -> String -> List (Html Msg) -> Html Msg
viewBar model heading tools =
    div [ class "h-12 shrink-0" ]
        [ div [ class "pointer-events-none absolute inset-y-0 start-0 z-10 w-1/2 will-change-transform" ]
            [ div [ class "sticky top-0 h-12" ]
                [ Html.header
                    [ Html.Attributes.id "bar"
                    , class "pointer-events-auto relative flex h-12 w-[200%] items-center gap-3 border-b px-4"
                    , class "before:absolute before:inset-x-0 before:-top-24 before:-bottom-px before:-z-10 before:bg-background/85 before:backdrop-blur"
                    ]
                    [ Sidebar.trigger [ class "-ms-1 text-muted-foreground" ] model.sidebar SidebarChanged
                    , Html.h1 [ class "text-sm font-medium" ] [ text heading ]
                    , div [ class "ms-auto flex min-w-0 flex-1 items-center justify-end gap-3" ] tools
                    ]
                ]
            ]
        ]


viewContent : List (Html Msg) -> Html Msg
viewContent children =
    Html.section [ class "flex min-w-0 flex-1 flex-col" ] children


notice : String -> String -> List (Html Msg) -> Html Msg
notice heading body actions =
    Card.new
        |> Card.title [ text heading ]
        |> Card.description [ text body ]
        |> (if List.isEmpty actions then
                identity

            else
                Card.footer [ class "gap-2" ] actions
           )
        |> Card.toHtml [ class "w-full max-w-sm" ]


retry : Html Msg
retry =
    Button.new
        |> Button.text "Try again"
        |> Button.outline
        |> Button.small
        |> Button.toHtml [ onClick SessionRetried ]


notFoundOptions : Session -> List (Html Msg)
notFoundOptions session =
    let
        option path label =
            Button.new
                |> Button.text label
                |> Button.outline
                |> Button.toAnchor [ href (Path.toString path), class "h-10 flex-1" ]
    in
    case session of
        Unknown ->
            []

        Unavailable _ ->
            [ Button.new
                |> Button.text "Try again"
                |> Button.outline
                |> Button.toHtml [ onClick SessionRetried, class "h-10 flex-1" ]
            ]

        SignedOut ->
            [ option Path.SignIn "Log in"
            , option Path.SignUp "Create an account"
            ]

        SignedIn _ ->
            [ option Path.Account "Account" ]


viewAuthTabs : Path.Path -> Html Msg
viewAuthTabs current =
    Tabs.new
        |> Tabs.id "auth"
        |> Tabs.onSelect PathSelected
        |> Tabs.value current
        |> Tabs.tabs
            [ Tabs.tab Path.SignIn [ text "Log in" ] []
            , Tabs.tab Path.SignUp [ text "Create an account" ] []
            ]
        |> Tabs.toHtml
            [ attribute "aria-label" "Log in or create an account"
            , class "*:data-[slot=tabs-list]:w-full"
            ]
