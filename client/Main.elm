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
import Path
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
    , ceremony : Ceremony
    , page : Page
    }


type Passkeys
    = Loading
    | Loaded (List Api.Passkey)
    | NotLoaded String


type alias DashboardState =
    { passkeys : Passkeys
    , confirming : Maybe String
    , removalProblem : Maybe String
    }


type Ceremony
    = Resting (Maybe String)
    | Waiting


type Session
    = Unknown
    | Unavailable String
    | SignedOut
    | SignedIn Api.Account


type Page
    = Home
    | SignUp SignUpForm
    | SignIn SignInForm
    | Verify Verification
    | Dashboard DashboardState
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
    = Open Attempt
    | Sent


type Attempt
    = Editing (Maybe String)
    | Submitting


type Verification
    = Looking String
    | Found String Api.Verification Acceptance
    | Accepted
    | Refused String


type Acceptance
    = Asking
    | Accepting


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
        , ceremony = Resting Nothing
        , page = NotFound
        }
        |> Tuple.mapSecond (\effect -> Batch [ Request (ApiClient.request Api.session () GotSession), effect ])


open : Url -> Model -> ( Model, Effect )
open url opened =
    let
        model =
            { opened | ceremony = Resting Nothing }
    in
    case Path.parse url of
        Just Path.Home ->
            ( { model | page = Home, health = Checking (lastReport model.health) }, checkHealth )

        Just Path.SignUp ->
            ( { model | page = SignUp { email = "", username = "", state = Open (Editing Nothing) } }, NoEffect )

        Just Path.SignIn ->
            ( { model | page = SignIn { email = "", state = Open (Editing Nothing) } }, NoEffect )

        Just Path.Dashboard ->
            ( { model | page = Dashboard { passkeys = Loading, confirming = Nothing, removalProblem = Nothing } }
            , if model.session == SignedOut then
                PushUrl (Path.toString Path.SignIn)

              else
                Request (ApiClient.request Api.passkeys () GotPasskeys)
            )

        Just (Path.Verify (Just token)) ->
            ( { model | page = Verify (Looking token) }
            , Request
                (ApiClient.request Api.verification () GotVerification
                    |> ApiClient.withToken token
                )
            )

        Just (Path.Verify Nothing) ->
            ( { model | page = Verify (Refused missingToken) }, NoEffect )

        Nothing ->
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
    | GotHealth (Result ApiClient.Error Api.Health)
    | CheckHealth
    | ThemeSelected Theme
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | EmailChanged String
    | UsernameChanged String
    | SignUpSubmitted
    | GotSignUp (Result ApiClient.Error ())
    | GotVerification (Result ApiClient.Error Api.Verification)
    | VerificationAccepted
    | GotAccount (Result ApiClient.Error Api.Account)
    | GotSession (Result ApiClient.Error Api.Account)
    | SessionRetried
    | SignInEmailChanged String
    | SignInSubmitted
    | GotSignIn (Result ApiClient.Error ())
    | SignOutClicked
    | GotSignOut (Result ApiClient.Error ())
    | GotPasskeys (Result ApiClient.Error (List Api.Passkey))
    | PasskeyRequested
    | GotPasskeyOptions (Result ApiClient.Error Api.PasskeyOptions)
    | GotPasskey (Result ApiClient.Error Api.Passkey)
    | PasskeyRemoveRequested String
    | PasskeyRemoveCancelled
    | PasskeyRemoveConfirmed String
    | GotPasskeyRemoved String (Result ApiClient.Error ())
    | PasskeySignInRequested
    | GotSignInOptions (Result ApiClient.Error Api.SignInOptions)
    | GotPasskeySignIn (Result ApiClient.Error Api.Account)


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        GotInterop (Ok (Interop.SystemThemeChanged systemDark)) ->
            ( { model | systemDark = systemDark }, NoEffect )

        GotInterop (Ok (Interop.PasskeyCreated attestation)) ->
            if model.ceremony == Waiting then
                ( model, Request (ApiClient.request Api.addPasskey attestation GotPasskey) )

            else
                ( model, NoEffect )

        GotInterop (Ok (Interop.PasskeyUsed assertion)) ->
            if model.ceremony == Waiting then
                ( model, Request (ApiClient.request Api.passkeySignIn assertion GotPasskeySignIn) )

            else
                ( model, NoEffect )

        GotInterop (Ok (Interop.PasskeyFailed name)) ->
            ( { model
                | ceremony =
                    Resting
                        (Just
                            (case model.page of
                                SignIn _ ->
                                    signInProblem name

                                _ ->
                                    passkeyProblem name
                            )
                        )
              }
            , NoEffect
            )

        GotInterop (Err _) ->
            ( model, NoEffect )

        GotHealth (Ok health) ->
            ( { model | health = Checked health }, NoEffect )

        GotHealth (Err error) ->
            ( { model | health = Unreachable (ApiClient.errorToString error) }, NoEffect )

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
                    if signUp.state == Open Submitting then
                        ( model, NoEffect )

                    else
                        ( { model | page = SignUp { signUp | state = Open Submitting } }
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

                        Err error ->
                            { signUp | state = Open (Editing (Just (ApiClient.errorToString error))) }
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
                                    Verify (Found token found Asking)

                                Err error ->
                                    Verify (Refused (ApiClient.errorToString error))
                      }
                    , NoEffect
                    )

                _ ->
                    ( model, NoEffect )

        VerificationAccepted ->
            case model.page of
                Verify (Found token found Asking) ->
                    ( { model | page = Verify (Found token found Accepting) }
                    , Request
                        (ApiClient.request Api.verify () GotAccount
                            |> ApiClient.withToken token
                        )
                    )

                _ ->
                    ( model, NoEffect )

        GotAccount result ->
            case ( model.page, result ) of
                ( Verify (Found _ found Accepting), Ok account ) ->
                    if found.newAccount then
                        ( { model | session = SignedIn account, page = Verify Accepted }, NoEffect )

                    else
                        ( { model | session = SignedIn account }, PushUrl (Path.toString Path.Dashboard) )

                ( Verify (Found _ _ Accepting), Err error ) ->
                    ( { model | page = Verify (Refused (ApiClient.errorToString error)) }, NoEffect )

                _ ->
                    ( model, NoEffect )

        GotSession (Ok account) ->
            ( { model | session = SignedIn account }, NoEffect )

        GotSession (Err error) ->
            case ( model.session, error ) of
                ( SignedIn _, _ ) ->
                    ( model, NoEffect )

                ( _, ApiClient.Unauthorized _ ) ->
                    ( { model | session = SignedOut }
                    , case model.page of
                        Dashboard _ ->
                            PushUrl (Path.toString Path.SignIn)

                        _ ->
                            NoEffect
                    )

                ( _, ApiClient.Failed message ) ->
                    ( { model | session = Unavailable message }, NoEffect )

        SessionRetried ->
            case model.page of
                Dashboard dashboard ->
                    ( { model | session = Unknown, page = Dashboard { dashboard | passkeys = Loading } }
                    , Batch
                        [ Request (ApiClient.request Api.session () GotSession)
                        , Request (ApiClient.request Api.passkeys () GotPasskeys)
                        ]
                    )

                _ ->
                    ( { model | session = Unknown }, Request (ApiClient.request Api.session () GotSession) )

        SignInEmailChanged email ->
            ( editSignIn (\signIn -> { signIn | email = email }) model, NoEffect )

        SignInSubmitted ->
            case model.page of
                SignIn signIn ->
                    if signIn.state == Open Submitting then
                        ( model, NoEffect )

                    else
                        ( { model | page = SignIn { signIn | state = Open Submitting } }
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

                        Err error ->
                            { signIn | state = Open (Editing (Just (ApiClient.errorToString error))) }
                )
                model
            , NoEffect
            )

        SignOutClicked ->
            ( model, Request (ApiClient.request Api.signOut () GotSignOut) )

        GotSignOut (Ok ()) ->
            ( { model | session = SignedOut }, PushUrl (Path.toString Path.Home) )

        GotSignOut (Err _) ->
            ( model, NoEffect )

        GotPasskeys (Ok found) ->
            ( editDashboard (\dashboard -> { dashboard | passkeys = Loaded found }) model, NoEffect )

        GotPasskeys (Err error) ->
            ( editDashboard (\dashboard -> { dashboard | passkeys = NotLoaded (ApiClient.errorToString error) }) model
            , NoEffect
            )

        PasskeyRequested ->
            if model.ceremony == Waiting then
                ( model, NoEffect )

            else
                ( { model | ceremony = Waiting }
                , Request (ApiClient.request Api.passkeyOptions () GotPasskeyOptions)
                )

        GotPasskeyOptions (Ok options) ->
            ( model, SendToJs (Interop.CreatePasskey options) )

        GotPasskeyOptions (Err error) ->
            ( { model | ceremony = Resting (Just (ApiClient.errorToString error)) }, NoEffect )

        GotPasskey (Ok passkey) ->
            ( { model | ceremony = Resting Nothing }
                |> editDashboard
                    (\dashboard ->
                        case dashboard.passkeys of
                            Loaded found ->
                                { dashboard | passkeys = Loaded (found ++ [ passkey ]) }

                            _ ->
                                dashboard
                    )
            , if model.page == Verify Accepted then
                PushUrl (Path.toString Path.Dashboard)

              else
                NoEffect
            )

        GotPasskey (Err error) ->
            ( { model | ceremony = Resting (Just (ApiClient.errorToString error)) }, NoEffect )

        PasskeyRemoveRequested id ->
            ( editDashboard (\dashboard -> { dashboard | confirming = Just id }) model, NoEffect )

        PasskeyRemoveCancelled ->
            ( editDashboard (\dashboard -> { dashboard | confirming = Nothing }) model, NoEffect )

        PasskeyRemoveConfirmed id ->
            ( editDashboard (\dashboard -> { dashboard | confirming = Nothing, removalProblem = Nothing }) model
            , Request (ApiClient.request (Api.deletePasskey id) () (GotPasskeyRemoved id))
            )

        GotPasskeyRemoved id (Ok ()) ->
            ( editDashboard
                (\dashboard ->
                    case dashboard.passkeys of
                        Loaded found ->
                            { dashboard | passkeys = Loaded (List.filter (\passkey -> passkey.id /= id) found) }

                        _ ->
                            dashboard
                )
                model
            , NoEffect
            )

        GotPasskeyRemoved _ (Err error) ->
            ( editDashboard (\dashboard -> { dashboard | removalProblem = Just (ApiClient.errorToString error) }) model
            , NoEffect
            )

        PasskeySignInRequested ->
            if model.ceremony == Waiting then
                ( model, NoEffect )

            else
                ( { model | ceremony = Waiting }
                , Request (ApiClient.request Api.passkeySignInOptions () GotSignInOptions)
                )

        GotSignInOptions (Ok options) ->
            ( model, SendToJs (Interop.GetPasskey options) )

        GotSignInOptions (Err error) ->
            ( { model | ceremony = Resting (Just (ApiClient.errorToString error)) }, NoEffect )

        GotPasskeySignIn (Ok account) ->
            ( { model | session = SignedIn account }, PushUrl (Path.toString Path.Dashboard) )

        GotPasskeySignIn (Err error) ->
            ( { model | ceremony = Resting (Just (ApiClient.errorToString error)) }, NoEffect )

        ThemeSelected theme ->
            ( { model | theme = theme }
            , SendToJs
                (Interop.WriteToLocalStorage
                    { key = "theme"
                    , value = Interop.themeToString theme
                    }
                )
            )


passkeyProblem : String -> String
passkeyProblem name =
    case name of
        "NotAllowedError" ->
            "The passkey was not created. Try again when you're ready."

        "InvalidStateError" ->
            "This device already has a passkey for this account."

        "NotSupportedError" ->
            "This browser can't create passkeys."

        _ ->
            "The passkey could not be created."


signInProblem : String -> String
signInProblem name =
    case name of
        "NotAllowedError" ->
            "No passkey was used. Try again when you're ready."

        "NotSupportedError" ->
            "This browser can't use passkeys."

        _ ->
            "The passkey could not be used."


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


editDashboard : (DashboardState -> DashboardState) -> Model -> Model
editDashboard edit model =
    case model.page of
        Dashboard dashboard ->
            { model | page = Dashboard (edit dashboard) }

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

        Dashboard _ ->
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

                    Unavailable _ ->
                        [ span [ class "text-sm text-muted-foreground" ] [ text "Can't check who is signed in." ]
                        , retry
                        ]

                    SignedOut ->
                        [ link Path.SignUp "Create an account"
                        , link Path.SignIn "Sign in"
                        ]

                    SignedIn _ ->
                        [ link Path.Dashboard "Dashboard" ]
                )
            ]

        SignUp signUp ->
            [ viewSignUp signUp ]

        SignIn signIn ->
            [ viewSignIn model.ceremony signIn ]

        Dashboard dashboard ->
            case model.session of
                SignedIn _ ->
                    [ viewDashboard model.session, viewPasskeys model.ceremony dashboard ]

                _ ->
                    [ viewDashboard model.session ]

        Verify verification ->
            [ viewVerification model.ceremony verification ]

        NotFound ->
            [ notice "Not found" "There is nothing at this address." [ link Path.Home "Go home" ] ]


link : Path.Path -> String -> Html Msg
link path label =
    Button.new
        |> Button.text label
        |> Button.link
        |> Button.toAnchor [ href (Path.toString path) ]


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


retry : Html Msg
retry =
    Button.new
        |> Button.text "Try again"
        |> Button.outline
        |> Button.small
        |> Button.toHtml [ onClick SessionRetried ]


viewSignUp : SignUpForm -> Html Msg
viewSignUp signUp =
    case signUp.state of
        Sent ->
            notice "Check your inbox"
                ("We sent a link to " ++ String.trim signUp.email ++ ". It works once and expires in 15 minutes.")
                []

        Open attempt ->
            viewSignUpForm signUp attempt


viewSignUpForm : SignUpForm -> Attempt -> Html Msg
viewSignUpForm signUp attempt =
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
                    , viewAttemptProblem attempt
                    , viewSubmit attempt
                    ]
                ]
            ]
        |> Card.footer [ class "text-sm text-muted-foreground" ]
            [ text "Already have an account?", link Path.SignIn "Sign in" ]
        |> Card.toHtml [ class "w-full max-w-sm" ]


viewSignIn : Ceremony -> SignInForm -> Html Msg
viewSignIn ceremony signIn =
    case signIn.state of
        Sent ->
            notice "Check your inbox"
                ("If " ++ String.trim signIn.email ++ " has an account, we sent it a link. It works once and expires in 15 minutes.")
                []

        Open attempt ->
            viewSignInForm ceremony signIn attempt


viewSignInForm : Ceremony -> SignInForm -> Attempt -> Html Msg
viewSignInForm ceremony signIn attempt =
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
                    , viewAttemptProblem attempt
                    , viewSubmit attempt
                    , Field.separator [ class "*:data-[slot=field-separator-content]:bg-card" ] [ text "Or" ]
                    , viewProblem ceremony
                    , Button.new
                        |> Button.text
                            (if ceremony == Waiting then
                                "Waiting for your device…"

                             else
                                "Sign in with a passkey"
                            )
                        |> Button.outline
                        |> Button.startIcon (Phosphor.key Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                        |> Button.toHtml [ type_ "button", onClick PasskeySignInRequested, disabled (ceremony == Waiting) ]
                    ]
                ]
            ]
        |> Card.footer [ class "text-sm text-muted-foreground" ]
            [ text "New here?", link Path.SignUp "Create an account" ]
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
                |> Card.footer [] [ link Path.Home "Server status" ]
                |> Card.toHtml [ class "w-full max-w-sm" ]

        Unavailable message ->
            notice "Can't check who is signed in" message [ retry ]

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


viewPasskeys : Ceremony -> DashboardState -> Html Msg
viewPasskeys ceremony { passkeys, confirming, removalProblem } =
    Card.new
        |> Card.title [ text "Passkeys" ]
        |> Card.description [ text "Your fingerprint, face or screen lock, instead of an emailed link." ]
        |> Card.action
            (Button.new
                |> Button.text "Add a passkey"
                |> Button.outline
                |> Button.small
                |> Button.toHtml [ onClick PasskeyRequested, disabled (ceremony == Waiting) ]
            )
        |> Card.content []
            [ div [ class "flex flex-col gap-3" ]
                [ case passkeys of
                    Loading ->
                        Skeleton.view [ class "h-5 w-48", attribute "aria-hidden" "true" ]

                    Loaded [] ->
                        p [ class "text-sm text-muted-foreground" ] [ text "No passkeys yet." ]

                    Loaded found ->
                        ul [ class "flex flex-col gap-3" ] (List.map (viewPasskey confirming) found)

                    NotLoaded message ->
                        p [ class "text-sm text-muted-foreground" ] [ text message ]
                , viewProblem ceremony
                , case removalProblem of
                    Just reason ->
                        Field.error [] [ text reason ]

                    Nothing ->
                        text ""
                ]
            ]
        |> Card.toHtml [ class "w-full max-w-sm" ]


viewPasskey : Maybe String -> Api.Passkey -> Html Msg
viewPasskey confirming passkey =
    let
        name =
            Maybe.withDefault "Passkey" passkey.name
    in
    li [ class "flex min-h-8 items-center gap-3" ]
        (if confirming == Just passkey.id then
            [ span [ class "text-sm font-medium" ] [ text ("Remove " ++ name ++ "?") ]
            , div [ class "ml-auto flex gap-2" ]
                [ Button.new
                    |> Button.text "Cancel"
                    |> Button.ghost
                    |> Button.small
                    |> Button.toHtml [ onClick PasskeyRemoveCancelled ]
                , Button.new
                    |> Button.text "Remove"
                    |> Button.destructive
                    |> Button.small
                    |> Button.toHtml [ onClick (PasskeyRemoveConfirmed passkey.id) ]
                ]
            ]

         else
            [ Phosphor.key Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
            , span [ class "font-medium" ] [ text name ]
            , span [ class "ml-auto text-sm text-muted-foreground tabular-nums" ]
                [ text ("Added " ++ String.left 10 passkey.createdAt) ]
            , Button.new
                |> Button.icon (Phosphor.trash Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                |> Button.label ("Remove " ++ name)
                |> Button.ghost
                |> Button.small
                |> Button.toHtml [ onClick (PasskeyRemoveRequested passkey.id) ]
            ]
        )


viewProblem : Ceremony -> Html Msg
viewProblem ceremony =
    case ceremony of
        Resting (Just reason) ->
            Field.error [] [ text reason ]

        _ ->
            text ""


viewAttemptProblem : Attempt -> Html Msg
viewAttemptProblem attempt =
    case attempt of
        Editing (Just reason) ->
            Field.error [] [ text reason ]

        _ ->
            text ""


viewSubmit : Attempt -> Html Msg
viewSubmit attempt =
    Button.new
        |> Button.text
            (case attempt of
                Submitting ->
                    "Sending the link…"

                Editing _ ->
                    "Email me a link"
            )
        |> Button.toHtml [ type_ "submit", disabled (attempt == Submitting) ]


viewVerification : Ceremony -> Verification -> Html Msg
viewVerification ceremony verification =
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

        Found _ found acceptance ->
            viewFound found acceptance

        Accepted ->
            Card.new
                |> Card.title [ text "Your account is ready" ]
                |> Card.description
                    [ text "Add a passkey to use your fingerprint, face or screen lock instead of an emailed link." ]
                |> Card.content []
                    [ div [ class "flex flex-col gap-5" ]
                        [ viewProblem ceremony
                        , Button.new
                            |> Button.text
                                (if ceremony == Waiting then
                                    "Waiting for your device…"

                                 else
                                    "Create a passkey"
                                )
                            |> Button.toHtml [ onClick PasskeyRequested, disabled (ceremony == Waiting) ]
                        ]
                    ]
                |> Card.footer [] [ link Path.Dashboard "Not now" ]
                |> Card.toHtml [ class "w-full max-w-sm" ]

        Refused reason ->
            notice "This link didn't work" reason [ link Path.SignUp "Sign up again" ]


viewFound : Api.Verification -> Acceptance -> Html Msg
viewFound found acceptance =
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
                , Button.new
                    |> Button.text
                        (if found.newAccount then
                            "Create my account"

                         else
                            "Sign in"
                        )
                    |> Button.toHtml [ onClick VerificationAccepted, disabled (acceptance == Accepting) ]
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
