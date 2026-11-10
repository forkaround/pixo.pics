module Main exposing (Effect(..), Model, Msg(..), init, main, update, view)

import Api
import ApiClient
import Browser
import Browser.Navigation as Navigation
import Html exposing (Html, a, dd, div, dl, dt, form, li, p, span, text, ul)
import Html.Attributes exposing (attribute, autocomplete, class, disabled, for, href, id, name, rel, required, target, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import InteropDefinitions as Interop exposing (Theme)
import InteropPorts
import Json.Decode as Decode
import Phosphor exposing (IconWeight(..))
import UI.Button as Button
import UI.Card as Card
import UI.Field as Field
import UI.Input as Input
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
        , subscriptions = \_ -> Sub.map GotInterop InteropPorts.toElm
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { theme : Theme
    , systemDark : Bool
    , health : Health
    , session : Session
    , page : Page
    }


type Session
    = Unknown
    | SignedOut
    | SignedIn Api.Account


type Page
    = Home
    | SignUp SignUpForm
    | SignIn SignInForm
    | Verify Verification
    | Dashboard
    | NotFound


type alias SignUpForm =
    { email : String
    , username : String
    , state : FormState
    }


type alias SignInForm =
    { email : String
    , state : FormState
    }


type FormState
    = Editing (Maybe String)
    | Submitting
    | Sent


type Verification
    = Looking String
    | Found String Api.Verification (Maybe String)
    | Accepting String Api.Verification
    | Refused String


type Health
    = Checking (Maybe Api.Health)
    | Checked Api.Health
    | Unreachable String


init : Decode.Value -> Url -> ( Model, Effect )
init flags url =
    let
        { theme, systemDark } =
            InteropPorts.decodeFlags flags
                |> Result.withDefault { theme = Nothing, systemDark = False }
    in
    open url
        { theme = Maybe.withDefault Interop.System theme
        , systemDark = systemDark
        , health = Checking Nothing
        , session = Unknown
        , page = NotFound
        }
        |> Tuple.mapSecond (\effect -> Batch [ Request (ApiClient.request Api.session () GotSession), effect ])


open : Url -> Model -> ( Model, Effect )
open url model =
    case ( url.path, url.fragment ) of
        ( "/", _ ) ->
            ( { model | page = Home, health = Checking (lastReport model.health) }, checkHealth )

        ( "/auth/signup", _ ) ->
            ( { model | page = SignUp { email = "", username = "", state = Editing Nothing } }, NoEffect )

        ( "/auth/signin", _ ) ->
            ( { model | page = SignIn { email = "", state = Editing Nothing } }, NoEffect )

        ( "/dashboard", _ ) ->
            ( { model | page = Dashboard }
            , if model.session == SignedOut then
                PushUrl "/auth/signin"

              else
                NoEffect
            )

        ( "/auth/verify", Just token ) ->
            if String.isEmpty token then
                ( { model | page = Verify (Refused missingToken) }, NoEffect )

            else
                ( { model | page = Verify (Looking token) }
                , Request
                    (ApiClient.request Api.verification () GotVerification
                        |> ApiClient.withToken token
                    )
                )

        ( "/auth/verify", Nothing ) ->
            ( { model | page = Verify (Refused missingToken) }, NoEffect )

        _ ->
            ( { model | page = NotFound }, NoEffect )


missingToken : String
missingToken =
    "This link is incomplete. Open the one from your email again."


checkHealth : Effect
checkHealth =
    Request (ApiClient.request Api.health () GotHealth)


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
    | GotHealth (Result String Api.Health)
    | CheckHealth
    | ThemeSelected Theme
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | EmailChanged String
    | UsernameChanged String
    | SignUpSubmitted
    | GotSignUp (Result String ())
    | GotVerification (Result String Api.Verification)
    | VerificationAccepted
    | GotAccount (Result String Api.Account)
    | GotSession (Result String Api.Account)
    | SignInEmailChanged String
    | SignInSubmitted
    | GotSignIn (Result String ())
    | SignOutClicked
    | GotSignOut (Result String ())


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        GotInterop (Ok (Interop.SystemThemeChanged systemDark)) ->
            ( { model | systemDark = systemDark }, NoEffect )

        GotInterop _ ->
            ( model, NoEffect )

        GotHealth (Ok health) ->
            ( { model | health = Checked health }, NoEffect )

        GotHealth (Err message) ->
            ( { model | health = Unreachable message }, NoEffect )

        CheckHealth ->
            ( { model | health = Checking (lastReport model.health) }, checkHealth )

        UrlRequested (Browser.Internal url) ->
            ( model, PushUrl (Url.toString url) )

        UrlRequested (Browser.External url) ->
            ( model, Load url )

        UrlChanged url ->
            open url model

        EmailChanged email ->
            ( editSignUp (\signUp -> { signUp | email = email }) model, NoEffect )

        UsernameChanged username ->
            ( editSignUp (\signUp -> { signUp | username = username }) model, NoEffect )

        SignUpSubmitted ->
            case model.page of
                SignUp signUp ->
                    if signUp.state == Submitting then
                        ( model, NoEffect )

                    else
                        ( { model | page = SignUp { signUp | state = Submitting } }
                        , Request
                            (ApiClient.request Api.signUp
                                { email = signUp.email, username = signUp.username }
                                GotSignUp
                            )
                        )

                _ ->
                    ( model, NoEffect )

        GotSignUp result ->
            ( editSignUp
                (\signUp ->
                    case result of
                        Ok () ->
                            { signUp | state = Sent }

                        Err message ->
                            { signUp | state = Editing (Just message) }
                )
                model
            , NoEffect
            )

        GotVerification result ->
            case model.page of
                Verify (Looking token) ->
                    ( { model
                        | page =
                            case result of
                                Ok found ->
                                    Verify (Found token found Nothing)

                                Err message ->
                                    Verify (Refused message)
                      }
                    , NoEffect
                    )

                _ ->
                    ( model, NoEffect )

        VerificationAccepted ->
            case model.page of
                Verify (Found token found _) ->
                    ( { model | page = Verify (Accepting token found) }
                    , Request
                        (ApiClient.request Api.verify () GotAccount
                            |> ApiClient.withToken token
                        )
                    )

                _ ->
                    ( model, NoEffect )

        GotAccount result ->
            case ( model.page, result ) of
                ( Verify (Accepting _ _), Ok account ) ->
                    ( { model | session = SignedIn account }, PushUrl "/dashboard" )

                ( Verify (Accepting _ _), Err message ) ->
                    ( { model | page = Verify (Refused message) }, NoEffect )

                _ ->
                    ( model, NoEffect )

        GotSession (Ok account) ->
            ( { model | session = SignedIn account }, NoEffect )

        GotSession (Err _) ->
            case model.session of
                SignedIn _ ->
                    ( model, NoEffect )

                _ ->
                    ( { model | session = SignedOut }
                    , if model.page == Dashboard then
                        PushUrl "/auth/signin"

                      else
                        NoEffect
                    )

        SignInEmailChanged email ->
            ( editSignIn (\signIn -> { signIn | email = email }) model, NoEffect )

        SignInSubmitted ->
            case model.page of
                SignIn signIn ->
                    if signIn.state == Submitting then
                        ( model, NoEffect )

                    else
                        ( { model | page = SignIn { signIn | state = Submitting } }
                        , Request (ApiClient.request Api.signIn { email = signIn.email } GotSignIn)
                        )

                _ ->
                    ( model, NoEffect )

        GotSignIn result ->
            ( editSignIn
                (\signIn ->
                    case result of
                        Ok () ->
                            { signIn | state = Sent }

                        Err message ->
                            { signIn | state = Editing (Just message) }
                )
                model
            , NoEffect
            )

        SignOutClicked ->
            ( model, Request (ApiClient.request Api.signOut () GotSignOut) )

        GotSignOut (Ok ()) ->
            ( { model | session = SignedOut }, PushUrl "/" )

        GotSignOut (Err _) ->
            ( model, NoEffect )

        ThemeSelected theme ->
            ( { model | theme = theme }
            , SendToJs
                (Interop.WriteToLocalStorage
                    { key = "theme"
                    , value = Interop.themeToString theme
                    }
                )
            )


editSignIn : (SignInForm -> SignInForm) -> Model -> Model
editSignIn edit model =
    case model.page of
        SignIn signIn ->
            { model | page = SignIn (edit signIn) }

        _ ->
            model


editSignUp : (SignUpForm -> SignUpForm) -> Model -> Model
editSignUp edit model =
    case model.page of
        SignUp signUp ->
            { model | page = SignUp (edit signUp) }

        _ ->
            model



-- EFFECTS


type Effect
    = NoEffect
    | Batch (List Effect)
    | Request (ApiClient.Request Msg)
    | SendToJs Interop.FromElm
    | PushUrl String
    | Load String


perform : Navigation.Key -> Effect -> Cmd Msg
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
            ]
            [ div [ class "flex min-h-screen flex-col items-center justify-center gap-4 bg-background p-6 text-foreground" ]
                (viewPage model ++ [ viewThemeTabs model.theme ])
            ]
        ]
    }


title : Page -> String
title page =
    case page of
        Home ->
            "pixo"

        SignUp _ ->
            "Sign up · pixo"

        SignIn _ ->
            "Sign in · pixo"

        Dashboard ->
            "Dashboard · pixo"

        Verify _ ->
            "Verify · pixo"

        NotFound ->
            "Not found · pixo"


viewPage : Model -> List (Html Msg)
viewPage model =
    case model.page of
        Home ->
            [ viewHealth model.health
            , div [ class "flex h-8 items-center gap-2" ]
                (case model.session of
                    Unknown ->
                        [ Skeleton.view [ class "h-4 w-40", attribute "aria-hidden" "true" ] ]

                    SignedOut ->
                        [ link "/auth/signup" "Create an account"
                        , link "/auth/signin" "Sign in"
                        ]

                    SignedIn _ ->
                        [ link "/dashboard" "Dashboard" ]
                )
            ]

        SignUp signUp ->
            [ viewSignUp signUp ]

        SignIn signIn ->
            [ viewSignIn signIn ]

        Dashboard ->
            [ viewDashboard model.session ]

        Verify verification ->
            [ viewVerification verification ]

        NotFound ->
            [ notice "Not found" "There is nothing at this address." [ link "/" "Go home" ] ]


link : String -> String -> Html Msg
link url label =
    Button.new
        |> Button.text label
        |> Button.link
        |> Button.toAnchor [ href url ]


notice : String -> String -> List (Html Msg) -> Html Msg
notice heading body actions =
    Card.new
        |> Card.title [ text heading ]
        |> Card.description [ text body ]
        |> (if List.isEmpty actions then
                identity

            else
                Card.footer [] actions
           )
        |> Card.toHtml [ class "w-full max-w-sm" ]


viewSignUp : SignUpForm -> Html Msg
viewSignUp signUp =
    case signUp.state of
        Sent ->
            notice "Check your inbox"
                ("We sent a link to " ++ String.trim signUp.email ++ ". It works once and expires in 15 minutes.")
                []

        Editing problem ->
            viewSignUpForm signUp False problem

        Submitting ->
            viewSignUpForm signUp True Nothing


viewSignUpForm : SignUpForm -> Bool -> Maybe String -> Html Msg
viewSignUpForm signUp busy problem =
    Card.new
        |> Card.title [ text "Create your account" ]
        |> Card.description [ text "We'll email you a link to confirm it's you. No password needed." ]
        |> Card.content []
            [ form [ onSubmit SignUpSubmitted ]
                [ Field.group []
                    [ Field.view []
                        [ Field.label [ for "email" ] [ text "Email" ]
                        , Input.view
                            [ id "email"
                            , name "email"
                            , type_ "email"
                            , autocomplete True
                            , required True
                            , value signUp.email
                            , onInput EmailChanged
                            ]
                        ]
                    , Field.view []
                        [ Field.label [ for "username" ] [ text "Username" ]
                        , Input.view
                            [ id "username"
                            , name "username"
                            , attribute "autocapitalize" "none"
                            , attribute "spellcheck" "false"
                            , required True
                            , value signUp.username
                            , onInput UsernameChanged
                            ]
                        , Field.description [] [ text "3 to 20 letters, digits or underscores." ]
                        ]
                    , case problem of
                        Just reason ->
                            Field.error [] [ text reason ]

                        Nothing ->
                            text ""
                    , Button.new
                        |> Button.text
                            (if busy then
                                "Sending the link…"

                             else
                                "Email me a link"
                            )
                        |> Button.toHtml [ type_ "submit", disabled busy ]
                    ]
                ]
            ]
        |> Card.footer [ class "text-sm text-muted-foreground" ]
            [ text "Already have an account?", link "/auth/signin" "Sign in" ]
        |> Card.toHtml [ class "w-full max-w-sm" ]


viewSignIn : SignInForm -> Html Msg
viewSignIn signIn =
    case signIn.state of
        Sent ->
            notice "Check your inbox"
                ("If " ++ String.trim signIn.email ++ " has an account, we sent it a link. It works once and expires in 15 minutes.")
                []

        Editing problem ->
            viewSignInForm signIn False problem

        Submitting ->
            viewSignInForm signIn True Nothing


viewSignInForm : SignInForm -> Bool -> Maybe String -> Html Msg
viewSignInForm signIn busy problem =
    Card.new
        |> Card.title [ text "Sign in" ]
        |> Card.description [ text "We'll email you a link. No password needed." ]
        |> Card.content []
            [ form [ onSubmit SignInSubmitted ]
                [ Field.group []
                    [ Field.view []
                        [ Field.label [ for "email" ] [ text "Email" ]
                        , Input.view
                            [ id "email"
                            , name "email"
                            , type_ "email"
                            , autocomplete True
                            , required True
                            , value signIn.email
                            , onInput SignInEmailChanged
                            ]
                        ]
                    , case problem of
                        Just reason ->
                            Field.error [] [ text reason ]

                        Nothing ->
                            text ""
                    , Button.new
                        |> Button.text
                            (if busy then
                                "Sending the link…"

                             else
                                "Email me a link"
                            )
                        |> Button.toHtml [ type_ "submit", disabled busy ]
                    ]
                ]
            ]
        |> Card.footer [ class "text-sm text-muted-foreground" ]
            [ text "New here?", link "/auth/signup" "Create an account" ]
        |> Card.toHtml [ class "w-full max-w-sm" ]


viewDashboard : Session -> Html Msg
viewDashboard session =
    case session of
        SignedIn account ->
            Card.new
                |> Card.title [ text ("Welcome, " ++ account.username) ]
                |> Card.description [ text ("Signed in as " ++ account.email) ]
                |> Card.action
                    (Button.new
                        |> Button.text "Sign out"
                        |> Button.outline
                        |> Button.small
                        |> Button.toHtml [ onClick SignOutClicked ]
                    )
                |> Card.content [ class "text-sm text-muted-foreground" ]
                    [ text "Your dashboard is empty for now." ]
                |> Card.footer [] [ link "/" "Server status" ]
                |> Card.toHtml [ class "w-full max-w-sm" ]

        _ ->
            Card.new
                |> Card.title [ text "Dashboard" ]
                |> Card.content [ attribute "aria-hidden" "true" ]
                    [ div [ class "flex flex-col gap-3" ]
                        [ Skeleton.view [ class "h-4 w-40" ]
                        , Skeleton.view [ class "h-4 w-56" ]
                        ]
                    ]
                |> Card.toHtml [ class "w-full max-w-sm" ]


viewVerification : Verification -> Html Msg
viewVerification verification =
    case verification of
        Looking _ ->
            Card.new
                |> Card.title [ text "Checking your link" ]
                |> Card.content [ attribute "aria-hidden" "true" ]
                    [ div [ class "flex flex-col gap-3" ]
                        [ Skeleton.view [ class "h-4 w-40" ]
                        , Skeleton.view [ class "h-4 w-56" ]
                        , Skeleton.view [ class "h-8 w-full" ]
                        ]
                    ]
                |> Card.toHtml [ class "w-full max-w-sm" ]

        Found _ found problem ->
            viewFound found False problem

        Accepting _ found ->
            viewFound found True Nothing

        Refused reason ->
            notice "This link didn't work" reason [ link "/auth/signup" "Sign up again" ]


viewFound : Api.Verification -> Bool -> Maybe String -> Html Msg
viewFound found busy problem =
    Card.new
        |> Card.title
            [ text
                (if found.newAccount then
                    "Create this account?"

                 else
                    "Sign in to this account?"
                )
            ]
        |> Card.description [ text "This link was requested for:" ]
        |> Card.content []
            [ div [ class "flex flex-col gap-5" ]
                [ dl [ class "grid grid-cols-[auto_1fr] gap-x-4 gap-y-2 text-sm" ]
                    [ dt [ class "text-muted-foreground" ] [ text "Username" ]
                    , dd [ class "font-medium" ] [ text found.username ]
                    , dt [ class "text-muted-foreground" ] [ text "Email" ]
                    , dd [ class "font-medium break-all" ] [ text found.email ]
                    ]
                , case problem of
                    Just reason ->
                        Field.error [] [ text reason ]

                    Nothing ->
                        text ""
                , Button.new
                    |> Button.text
                        (if found.newAccount then
                            "Create my account"

                         else
                            "Sign in"
                        )
                    |> Button.toHtml [ onClick VerificationAccepted, disabled busy ]
                , p [ class "text-sm text-muted-foreground" ]
                    [ text "Not you? Close this page: nothing happens until you accept." ]
                ]
            ]
        |> Card.toHtml [ class "w-full max-w-sm" ]


viewHealth : Health -> Html Msg
viewHealth health =
    Card.new
        |> Card.title [ text "Server" ]
        |> Card.description [ text "GET /api/health" ]
        |> Card.action
            (Button.new
                |> Button.text "Refresh"
                |> Button.outline
                |> Button.small
                |> Button.toHtml [ onClick CheckHealth ]
            )
        |> Card.content []
            [ case health of
                Checking Nothing ->
                    viewRows (List.map (\name -> row pendingDot name latencySkeleton Nothing) Api.checkNames)

                Checking (Just report) ->
                    viewRows (List.map (viewCheck (always pendingDot)) (Api.checks report))

                Checked report ->
                    viewRows (List.map (viewCheck statusDot) (Api.checks report))

                Unreachable message ->
                    viewRows [ row "bg-red-500" "Server" (text "") (Just message) ]
            ]
        |> Card.footer [ class "text-xs text-muted-foreground" ] (viewRelease health)
        |> Card.toHtml [ class "w-full max-w-sm" ]


pendingDot : String
pendingDot =
    "animate-pulse bg-muted-foreground"


lastReport : Health -> Maybe Api.Health
lastReport health =
    case health of
        Checking report ->
            report

        Checked report ->
            Just report

        Unreachable _ ->
            Nothing


viewRelease : Health -> List (Html Msg)
viewRelease health =
    case ( health, Maybe.map .release (lastReport health) ) of
        ( Checking Nothing, _ ) ->
            [ div [ class "flex items-center gap-2", attribute "aria-hidden" "true" ]
                [ Skeleton.view [ class "h-4 w-24" ]
                , Skeleton.view [ class "h-4 w-20" ]
                ]
            ]

        ( _, Just release ) ->
            case
                List.filterMap identity
                    [ Maybe.map (viewReleaseId release.releaseUrl) release.releaseId
                    , Maybe.map (\region -> [ text ("Region " ++ region) ]) release.region
                    ]
            of
                [] ->
                    [ placeholder ]

                parts ->
                    [ span [] (List.concat (List.intersperse [ text " · " ] parts)) ]

        ( _, Nothing ) ->
            [ placeholder ]


placeholder : Html Msg
placeholder =
    span [ class "invisible", attribute "aria-hidden" "true" ] [ text "Release" ]


viewReleaseId : Maybe String -> String -> List (Html Msg)
viewReleaseId url id =
    case url of
        Just href_ ->
            [ text "Release "
            , a
                [ href href_
                , target "_blank"
                , rel "noopener noreferrer"
                , class "font-mono underline underline-offset-2 hover:text-foreground"
                ]
                [ text (shortRelease id) ]
            ]

        Nothing ->
            [ text ("Release " ++ shortRelease id) ]


shortRelease : String -> String
shortRelease id =
    if String.startsWith "dpl_" id then
        id

    else
        String.left 7 id


viewRows : List (Html Msg) -> Html Msg
viewRows rows =
    ul [ class "flex flex-col gap-3", attribute "role" "status" ] rows


viewCheck : (Api.Status -> String) -> ( String, Api.Check ) -> Html Msg
viewCheck toDot ( name, check ) =
    row (toDot check.status) name (latency check.latencyMs) check.output


latency : Maybe Int -> Html Msg
latency ms =
    text (Maybe.withDefault "" (Maybe.map (\n -> String.fromInt n ++ " ms") ms))


latencySkeleton : Html Msg
latencySkeleton =
    Skeleton.view [ class "h-4 w-12", attribute "aria-hidden" "true" ]


statusDot : Api.Status -> String
statusDot status =
    case status of
        Api.Pass ->
            "bg-green-500"

        Api.Warn ->
            "bg-yellow-500"

        Api.Fail ->
            "bg-red-500"


row : String -> String -> Html Msg -> Maybe String -> Html Msg
row dot name measured output =
    li [ class "flex flex-wrap items-center gap-x-3 gap-y-1" ]
        [ span [ class ("size-3 shrink-0 rounded-full " ++ dot) ] []
        , span [ class "font-medium" ] [ text name ]
        , div [ class "ml-auto text-sm text-muted-foreground tabular-nums" ] [ measured ]
        , case output of
            Just message ->
                p [ class "w-full pl-6 text-sm text-muted-foreground" ] [ text message ]

            Nothing ->
                text ""
        ]


viewThemeTabs : Theme -> Html Msg
viewThemeTabs theme =
    Tabs.new
        |> Tabs.id "theme"
        |> Tabs.onSelect ThemeSelected
        |> Tabs.value theme
        |> Tabs.tabs
            [ themeTab Interop.Light Phosphor.sun "Light"
            , themeTab Interop.Dark Phosphor.moon "Dark"
            , themeTab Interop.System Phosphor.monitor "System"
            ]
        |> Tabs.toHtml [ attribute "aria-label" "Theme" ]


themeTab : Theme -> Phosphor.Icon -> String -> Tabs.Tab Theme Msg
themeTab theme icon label =
    Tabs.tab theme
        [ icon Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
        , span [ class "sr-only" ] [ text label ]
        ]
        []
