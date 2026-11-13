module Main exposing (Model, Msg(..), init, main, update, view)

import Api
import ApiClient
import Browser
import Browser.Events
import Browser.Navigation as Navigation
import Data.CredentialId exposing (CredentialId)
import Data.Email as Email
import Data.LinkToken exposing (LinkToken)
import Data.Username as Username
import Effect exposing (Effect(..))
import ExploreView
import Html exposing (Html, a, dd, div, dl, dt, form, li, p, span, text, ul)
import Html.Attributes exposing (attribute, autocomplete, class, disabled, for, href, id, name, required, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import InteropDefinitions as Interop exposing (Theme)
import InteropPorts
import Json.Decode as Decode
import Library
import LibraryView
import Logo
import ModelsView
import Path
import Phosphor exposing (IconWeight(..))
import Pipeline
import RelativeTime
import StudioView
import Svg.Attributes
import Task
import Time
import UI.AlertDialog as AlertDialog
import UI.Avatar as Avatar
import UI.Button as Button
import UI.Card as Card
import UI.DropdownMenu as DropdownMenu
import UI.Field as Field
import UI.InputGroup as InputGroup
import UI.Item as Item
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
    , ceremony : Ceremony
    , pipeline : Pipeline.Model
    , library : Library.Model
    , sidebar : Sidebar.State
    , sidebarProgress : Float
    , page : Page
    }


type Passkeys
    = Loading
    | Loaded (List Api.Passkey)
    | NotLoaded String


type alias AccountState =
    { passkeys : Passkeys
    , confirming : Maybe CredentialId
    , removalProblem : Maybe String
    , deletion : AccountDeletion
    , now : Maybe Time.Posix
    }


type AccountDeletion
    = KeepAccount
    | ConfirmDeletion (Maybe String)
    | DeletingAccount


type Ceremony
    = Resting (Maybe String)
    | Waiting


type Session
    = Unknown
    | Unavailable String
    | SignedOut
    | SignedIn Api.Account


type Page
    = Explore
    | Studio
    | Library
    | Settings Section
    | SignUp SignUpForm
    | SignIn SignInForm
    | Verify Verification
    | Passkey
    | NotFound


type Section
    = Models
    | Account AccountState


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
    = Looking LinkToken
    | Found LinkToken Api.Verification Acceptance
    | Refused String


type Acceptance
    = Asking
    | Accepting


init : Decode.Value -> Url -> ( Model, Effect Msg )
init flags url =
    let
        { theme, systemDark, entropy, models, canShare } =
            InteropPorts.decodeFlags flags
                |> Result.withDefault { theme = Nothing, systemDark = False, entropy = 0, models = Nothing, canShare = False }
    in
    open url
        { theme = Maybe.withDefault Interop.System theme
        , systemDark = systemDark
        , session = Unknown
        , ceremony = Resting Nothing
        , pipeline = Pipeline.init entropy (Maybe.withDefault [] (Maybe.map Pipeline.remembered models))
        , library = Library.init canShare
        , sidebar = Sidebar.init
        , sidebarProgress = 0
        , page = NotFound
        }
        |> Tuple.mapSecond (\effect -> Batch [ Request (ApiClient.request Api.session () GotSession), effect ])


open : Url -> Model -> ( Model, Effect Msg )
open url opened =
    let
        model =
            { opened | ceremony = Resting Nothing, sidebar = { open = opened.sidebar.open, openMobile = False } }
    in
    case Path.parse url of
        Path.Explore ->
            ( { model | page = Explore }, NoEffect )

        Path.Studio ->
            runPipeline { model | page = Studio } (Pipeline.enter model.pipeline)

        Path.Library ->
            runLibrary { model | page = Library } (Library.enter model.library)

        Path.Models ->
            runPipeline { model | page = Settings Models } (Pipeline.enter model.pipeline)

        Path.SignUp ->
            ( { model | page = SignUp { email = "", username = "", state = Open (Editing Nothing) } }, NoEffect )

        Path.SignIn ->
            ( { model | page = SignIn { email = "", state = Open (Editing Nothing) } }, NoEffect )

        Path.Account ->
            ( { model | page = Settings (Account { passkeys = Loading, confirming = Nothing, removalProblem = Nothing, deletion = KeepAccount, now = Nothing }) }
            , if model.session == SignedOut then
                PushUrl (Path.toString Path.SignIn)

              else
                Request (ApiClient.request Api.passkeys () GotPasskeys)
            )

        Path.Verify (Just token) ->
            ( { model | page = Verify (Looking token) }
            , Request
                (ApiClient.request Api.verification () GotVerification
                    |> ApiClient.withToken token
                )
            )

        Path.Verify Nothing ->
            ( { model | page = Verify (Refused missingToken) }, NoEffect )

        Path.Passkey ->
            ( { model | page = Passkey }
            , if model.session == SignedOut then
                PushUrl (Path.toString Path.SignIn)

              else
                NoEffect
            )

        Path.NotFound ->
            ( { model | page = NotFound }, NoEffect )


missingToken : String
missingToken =
    "This link is incomplete. Open the one from your email again."


runPipeline : Model -> ( Pipeline.Model, Maybe Pipeline.Command ) -> ( Model, Effect Msg )
runPipeline model ( pipeline, command ) =
    ( { model | pipeline = pipeline }
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
    | EmailChanged String
    | UsernameChanged String
    | SignUpSubmitted
    | GotSignUp (Result ApiClient.Error ())
    | GotVerification (Result ApiClient.Error Api.Verification)
    | VerificationAccepted
    | GotAccount (Result ApiClient.Error Api.Account)
    | GotPasskeyCheck (Result ApiClient.Error (List Api.Passkey))
    | GotSession (Result ApiClient.Error Api.Account)
    | SessionRetried
    | SignInEmailChanged String
    | SignInSubmitted
    | GotSignIn (Result ApiClient.Error ())
    | AccountDeleteRequested
    | AccountDeleteCancelled
    | AccountDeleteConfirmed
    | GotAccountDeleted (Result ApiClient.Error ())
    | SignOutClicked
    | GotSignOut (Result ApiClient.Error ())
    | GotTime Time.Posix
    | GotPasskeys (Result ApiClient.Error (List Api.Passkey))
    | PasskeyRequested
    | GotPasskeyOptions (Result ApiClient.Error Api.PasskeyOptions)
    | GotPasskey (Result ApiClient.Error Api.Passkey)
    | PasskeyRemoveRequested CredentialId
    | PasskeyRemoveCancelled
    | PasskeyRemoveConfirmed CredentialId
    | GotPasskeyRemoved CredentialId (Result ApiClient.Error ())
    | PasskeySignInRequested
    | GotSignInOptions (Result ApiClient.Error Api.SignInOptions)
    | GotPasskeySignIn (Result ApiClient.Error Api.Account)
    | PathSelected Path.Path
    | GotPipeline Pipeline.Msg
    | GotLibrary Library.Msg
    | SidebarChanged Sidebar.State
    | SidebarAnimated Float


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GotInterop InteropPorts.toElm
        , if model.sidebarProgress == sidebarTarget model.sidebar then
            Sub.none

          else
            Browser.Events.onAnimationFrameDelta SidebarAnimated
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

        GotInterop (Ok (Interop.PipelineEvent event)) ->
            runPipeline model (Pipeline.update (Pipeline.Happened event) model.pipeline)

        GotInterop (Ok (Interop.LibraryEvent event)) ->
            runLibrary model (Library.update (Library.Happened event) model.library)

        GotInterop (Err _) ->
            ( model, NoEffect )

        GotPipeline pipelineMsg ->
            runPipeline model (Pipeline.update pipelineMsg model.pipeline)

        GotLibrary libraryMsg ->
            runLibrary model (Library.update libraryMsg model.library)

        SidebarChanged sidebar ->
            ( { model | sidebar = sidebar }, NoEffect )

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

        PathSelected path ->
            ( model, PushUrl (Path.toString path) )

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
                ( Verify (Found _ _ Accepting), Ok account ) ->
                    ( { model | session = SignedIn account }
                    , Request (ApiClient.request Api.passkeys () GotPasskeyCheck)
                    )

                ( Verify (Found _ _ Accepting), Err error ) ->
                    ( { model | page = Verify (Refused (ApiClient.errorToString error)) }, NoEffect )

                _ ->
                    ( model, NoEffect )

        GotPasskeyCheck result ->
            case model.page of
                Verify (Found _ _ Accepting) ->
                    ( model
                    , PushUrl
                        (Path.toString
                            (case result of
                                Ok [] ->
                                    Path.Passkey

                                _ ->
                                    Path.Explore
                            )
                        )
                    )

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
                        Settings (Account _) ->
                            PushUrl (Path.toString Path.SignIn)

                        Passkey ->
                            PushUrl (Path.toString Path.SignIn)

                        _ ->
                            NoEffect
                    )

                ( _, ApiClient.Failed message ) ->
                    ( { model | session = Unavailable message }, NoEffect )

        SessionRetried ->
            case model.page of
                Settings (Account account) ->
                    ( { model | session = Unknown, page = Settings (Account { account | passkeys = Loading }) }
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

        AccountDeleteRequested ->
            ( editAccount (\account -> { account | deletion = ConfirmDeletion Nothing }) model, NoEffect )

        AccountDeleteCancelled ->
            ( editAccount
                (\account ->
                    if account.deletion == DeletingAccount then
                        account

                    else
                        { account | deletion = KeepAccount }
                )
                model
            , NoEffect
            )

        AccountDeleteConfirmed ->
            case ( model.session, model.page ) of
                ( SignedIn _, Settings (Account ({ deletion } as account)) ) ->
                    case deletion of
                        ConfirmDeletion _ ->
                            ( { model | page = Settings (Account { account | deletion = DeletingAccount }) }
                            , Request (ApiClient.request Api.deleteAccount () GotAccountDeleted)
                            )

                        _ ->
                            ( model, NoEffect )

                _ ->
                    ( model, NoEffect )

        GotAccountDeleted (Ok ()) ->
            ( { model | session = SignedOut, ceremony = Resting Nothing }, PushUrl (Path.toString Path.Explore) )

        GotAccountDeleted (Err (ApiClient.Unauthorized _)) ->
            ( { model | session = SignedOut, ceremony = Resting Nothing }, PushUrl (Path.toString Path.SignIn) )

        GotAccountDeleted (Err error) ->
            ( editAccount (\account -> { account | deletion = ConfirmDeletion (Just (ApiClient.errorToString error)) }) model, NoEffect )

        SignOutClicked ->
            ( model, Request (ApiClient.request Api.signOut () GotSignOut) )

        GotSignOut (Ok ()) ->
            ( { model | session = SignedOut }, PushUrl (Path.toString Path.Explore) )

        GotSignOut (Err _) ->
            ( model, NoEffect )

        GotTime now ->
            ( editAccount (\account -> { account | now = Just now }) model, NoEffect )

        GotPasskeys (Ok found) ->
            ( editAccount (\account -> { account | passkeys = Loaded found }) model, ReadTime GotTime )

        GotPasskeys (Err error) ->
            ( editAccount (\account -> { account | passkeys = NotLoaded (ApiClient.errorToString error) }) model
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
                |> editAccount
                    (\account ->
                        case account.passkeys of
                            Loaded found ->
                                { account | passkeys = Loaded (found ++ [ passkey ]) }

                            _ ->
                                account
                    )
            , if model.page == Passkey then
                PushUrl (Path.toString Path.Explore)

              else
                ReadTime GotTime
            )

        GotPasskey (Err error) ->
            ( { model | ceremony = Resting (Just (ApiClient.errorToString error)) }, NoEffect )

        PasskeyRemoveRequested id ->
            ( editAccount (\account -> { account | confirming = Just id }) model, NoEffect )

        PasskeyRemoveCancelled ->
            ( editAccount (\account -> { account | confirming = Nothing }) model, NoEffect )

        PasskeyRemoveConfirmed id ->
            ( editAccount (\account -> { account | confirming = Nothing, removalProblem = Nothing }) model
            , Request (ApiClient.request (Api.deletePasskey id) () (GotPasskeyRemoved id))
            )

        GotPasskeyRemoved id (Ok ()) ->
            ( editAccount
                (\account ->
                    case account.passkeys of
                        Loaded found ->
                            { account | passkeys = Loaded (List.filter (\passkey -> passkey.id /= id) found) }

                        _ ->
                            account
                )
                model
            , NoEffect
            )

        GotPasskeyRemoved _ (Err error) ->
            ( editAccount (\account -> { account | removalProblem = Just (ApiClient.errorToString error) }) model
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
            ( { model | session = SignedIn account }, PushUrl (Path.toString Path.Explore) )

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
            "The passkey was not created."

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
            "No passkey was used."

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


editAccount : (AccountState -> AccountState) -> Model -> Model
editAccount edit model =
    case model.page of
        Settings (Account account) ->
            { model | page = Settings (Account (edit account)) }

        _ ->
            model



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

        Passkey ->
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
                [ viewAside "border-b md:w-80 md:border-e md:border-b-0" [ Html.map GotPipeline (StudioView.controls model.pipeline) ]
                , viewContent
                    [ StudioView.canvas { pipeline = GotPipeline, library = GotLibrary } model.library model.pipeline ]
                ]

        Library ->
            viewFrame model
                "Library"
                [ Html.map GotLibrary (LibraryView.search model.library) ]
                [ viewContent [ Html.map GotLibrary (LibraryView.view model.library) ] ]

        Explore ->
            viewFrame model "Explore" [] [ viewContent [ ExploreView.view (hero model.session) ] ]

        Settings section ->
            viewFrame model
                "Settings"
                []
                [ viewAside "md:w-56 md:pt-6"
                    [ Sidebar.group []
                        [ Sidebar.menu [ class "max-md:flex-row max-md:flex-wrap" ]
                            (viewDestination (section == Models) Path.Models Phosphor.cube "Models"
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
                        Models ->
                            viewSettingsPage "Models"
                                (text
                                    (if model.pipeline.engine == Pipeline.Unsupported then
                                        "They need WebGPU. Try a recent Chrome, Edge or Safari."

                                     else
                                        "Manage the models that run in your browser. Your prompts stay on this device."
                                    )
                                )
                                [ Html.map GotPipeline (ModelsView.view model.pipeline) ]

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
                                (viewAccount model.session model.ceremony account)
                    ]
                ]

        SignUp signUp ->
            viewPlain model.theme (viewAuthTabs Path.SignUp :: viewSignUp signUp)

        SignIn signIn ->
            viewPlain model.theme (viewAuthTabs Path.SignIn :: viewSignIn model.ceremony signIn)

        Verify verification ->
            viewPlain model.theme (viewVerification verification)

        Passkey ->
            viewPlain model.theme (viewEnrolment model.ceremony)

        NotFound ->
            viewPlain model.theme
                [ viewLead "Not found" "There is nothing at this address."
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
                        [ Svg.Attributes.class "h-4 w-auto max-w-none shrink-0" ]
                    ]
                , span [ class "sr-only" ] [ text "pixo" ]
                ]
            ]
        |> Sidebar.content
            [ Sidebar.group []
                [ Sidebar.menu []
                    (viewDestination (model.page == Explore) Path.Explore Phosphor.kanban "Explore"
                        :: viewDestination (model.page == Studio) Path.Studio Phosphor.stack "Studio"
                        :: viewDestination (model.page == Library) Path.Library Phosphor.slideshow "Library"
                        :: [ viewDestination (isSettings model.page) Path.Models Phosphor.sliders "Settings" ]
                    )
                ]
            ]
        |> Sidebar.footer
            [ Sidebar.menu [] [ viewSession model ] ]
        |> Sidebar.rail
        |> Sidebar.page [ class "min-w-0" ] page
        |> Sidebar.toHtml [ class "sidebar-morph bg-background text-foreground" ]


hero : Session -> ExploreView.Hero
hero session =
    case session of
        Unknown ->
            ExploreView.Reserved

        Unavailable _ ->
            ExploreView.Hidden

        SignedOut ->
            ExploreView.Shown

        SignedIn _ ->
            ExploreView.Hidden


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
            Sidebar.menuItem []
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
                [ div [ class "flex h-12 items-center gap-2 p-2 md:group-data-[collapsible=icon]:p-0" ]
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
            Sidebar.menuItem []
                [ viewUserMenu model.theme
                    { avatar = [ text (String.toUpper (String.left 2 (Username.toString account.username))) ]
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
        |> DropdownMenu.trigger (Sidebar.menuButtonAttributes ++ [ Sidebar.large ])
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
    Html.header
        [ class "sticky top-0 z-10 flex h-12 shrink-0 items-center gap-3 border-b bg-background/85 px-4 backdrop-blur" ]
        [ Sidebar.trigger [ class "-ms-1 text-muted-foreground" ] model.sidebar SidebarChanged
        , Html.h1 [ class "text-sm font-medium" ] [ text heading ]
        , div [ class "ms-auto flex min-w-0 flex-1 items-center justify-end gap-3" ] (tools ++ [ viewEntry model ])
        ]


viewEntry : Model -> Html Msg
viewEntry model =
    let
        entry path label attrs =
            Button.new
                |> Button.text label
                |> Button.ghost
                |> Button.toAnchor (href (Path.toString path) :: class "text-muted-foreground" :: attrs)

        links attrs =
            div (class "flex shrink-0 items-center gap-1" :: attrs)
                [ entry Path.SignIn "Log in" [ class "max-sm:hidden" ]
                , entry Path.SignUp "Sign up" []
                ]
    in
    case model.session of
        SignedOut ->
            links []

        Unknown ->
            links [ class "invisible", attribute "aria-hidden" "true" ]

        _ ->
            text ""


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


viewPlain : Theme -> List (Html Msg) -> Html Msg
viewPlain theme children =
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
            , Html.footer [ class "flex h-9 items-center" ] [ viewThemeTabs theme ]
            ]
        , div [ class "hidden p-4 ps-0 lg:block", attribute "aria-hidden" "true" ]
            [ div [ class "relative h-full overflow-hidden mask-b-from-55%" ]
                [ div [ class "absolute inset-x-0 top-0 columns-3 gap-4" ] ExploreView.mosaic ]
            ]
        ]


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


viewLead : String -> String -> Html Msg
viewLead heading body =
    div [ class "flex flex-col gap-2" ]
        [ Html.h1 [ class "text-2xl font-semibold tracking-tight" ] [ text heading ]
        , p [ class "text-sm text-pretty text-muted-foreground" ] [ text body ]
        ]


viewSent : String -> List (Html Msg)
viewSent body =
    [ viewLead "Check your inbox" body ]


viewSignUp : SignUpForm -> List (Html Msg)
viewSignUp signUp =
    case signUp.state of
        Sent ->
            viewSent ("We sent a link to " ++ String.trim signUp.email ++ ". It expires in 15 minutes.")

        Open attempt ->
            [ viewLead "Create your account" "We'll email you a link. No password needed."
            , form [ onSubmit SignUpSubmitted ]
                [ Field.group []
                    [ viewEmailField signUp.email EmailChanged
                    , Field.view []
                        [ Field.label [ for "username", class "sr-only" ] [ text "Username" ]
                        , InputGroup.view [ class "h-10" ]
                            [ InputGroup.input
                                [ id "username"
                                , name "username"
                                , Html.Attributes.placeholder "Username"
                                , class "font-medium"
                                , attribute "autocapitalize" "none"
                                , attribute "spellcheck" "false"
                                , required True
                                , value signUp.username
                                , onInput UsernameChanged
                                ]
                            , InputGroup.start [] [ Phosphor.at Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ] ]
                            ]
                        , Field.description [] [ text "3 to 20 letters, digits or underscores." ]
                        ]
                    , viewAttemptProblem attempt
                    , viewSubmit attempt
                    ]
                ]
            ]


viewSignIn : Ceremony -> SignInForm -> List (Html Msg)
viewSignIn ceremony signIn =
    case signIn.state of
        Sent ->
            viewSent ("If " ++ String.trim signIn.email ++ " has an account, we sent it a link. It expires in 15 minutes.")

        Open attempt ->
            [ viewLead "Log in to your account" "Use your passkey, or get a link by email."
            , form [ onSubmit SignInSubmitted ]
                [ Field.group []
                    [ viewProblem ceremony
                    , Button.new
                        |> Button.text
                            (if ceremony == Waiting then
                                "Waiting for your device…"

                             else
                                "Log in with a passkey"
                            )
                        |> Button.startIcon (Phosphor.fingerprint Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                        |> Button.toHtml
                            [ type_ "button"
                            , onClick PasskeySignInRequested
                            , disabled (ceremony == Waiting)
                            , class "h-10"
                            ]
                    , Field.separator [] [ text "or" ]
                    , viewEmailField signIn.email SignInEmailChanged
                    , viewAttemptProblem attempt
                    , Button.new
                        |> Button.text (submitLabel attempt)
                        |> Button.outline
                        |> Button.toHtml [ type_ "submit", disabled (attempt == Submitting), class "h-10" ]
                    ]
                ]
            ]


viewEmailField : String -> (String -> Msg) -> Html Msg
viewEmailField email toMsg =
    Field.view []
        [ Field.label [ for "email", class "sr-only" ] [ text "Email" ]
        , InputGroup.view [ class "h-10" ]
            [ InputGroup.input
                [ id "email"
                , name "email"
                , type_ "email"
                , Html.Attributes.placeholder "Email"
                , class "font-medium"
                , autocomplete True
                , required True
                , value email
                , onInput toMsg
                ]
            , InputGroup.start [] [ Phosphor.envelopeSimple Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ] ]
            ]
        ]


viewAccount : Session -> Ceremony -> AccountState -> List (Html Msg)
viewAccount session ceremony account =
    case session of
        SignedIn _ ->
            [ viewPasskeys session ceremony account, viewDeleteAccount (Just account.deletion) ]

        Unknown ->
            [ viewPasskeys session (Resting Nothing) { account | passkeys = Loading, removalProblem = Nothing }
            , viewDeleteAccount Nothing
            ]

        Unavailable message ->
            [ notice "Can't check who is signed in" message [ retry ] ]

        SignedOut ->
            []


viewDeleteAccount : Maybe AccountDeletion -> Html Msg
viewDeleteAccount deletion =
    div []
        [ Card.new
            |> Card.title [ Html.h3 [ class "text-destructive" ] [ text "Delete account" ] ]
            |> Card.description [ text "Permanently delete your account, passkeys and sessions. This cannot be undone." ]
            |> Card.content []
                [ p [ class "text-sm text-muted-foreground" ] [ text "Images saved in this browser will remain." ] ]
            |> Card.footer [ class "justify-end" ]
                [ Button.new
                    |> Button.text "Delete account"
                    |> Button.destructive
                    |> Button.small
                    |> (if deletion == Nothing then
                            viewButtonSkeleton []

                        else
                            Button.toHtml [ onClick AccountDeleteRequested, disabled (deletion == Just DeletingAccount) ]
                       )
                ]
            |> Card.toHtml [ class "w-full ring-destructive/40! [--card-spacing:--spacing(5)] sm:[--card-spacing:--spacing(6)]" ]
        , viewDeleteAccountDialog deletion
        ]


viewDeleteAccountDialog : Maybe AccountDeletion -> Html Msg
viewDeleteAccountDialog deletion =
    AlertDialog.new
        |> AlertDialog.id "delete-account"
        |> AlertDialog.open (deletion /= Nothing && deletion /= Just KeepAccount) AccountDeleteCancelled
        |> AlertDialog.busy (deletion == Just DeletingAccount)
        |> AlertDialog.small
        |> AlertDialog.media [ class "bg-destructive/10 text-destructive dark:bg-destructive/20" ]
            [ Phosphor.trash Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ] ]
        |> AlertDialog.title [ text "Are you sure?" ]
        |> AlertDialog.description
            [ text "This will permanently delete your account and cannot be undone."
            , case deletion of
                Just (ConfirmDeletion (Just reason)) ->
                    span [ class "mt-3 block text-destructive", attribute "role" "alert" ] [ text reason ]

                _ ->
                    text ""
            ]
        |> AlertDialog.cancel
            (Button.new
                |> Button.text "Cancel"
                |> Button.outline
                |> Button.small
            )
        |> AlertDialog.action AccountDeleteConfirmed
            (Button.new
                |> Button.text
                    (if deletion == Just DeletingAccount then
                        "Deleting…"

                     else
                        "Delete"
                    )
                |> Button.destructive
                |> Button.small
            )
        |> AlertDialog.toHtml []


viewPasskeys : Session -> Ceremony -> AccountState -> Html Msg
viewPasskeys session ceremony { passkeys, confirming, removalProblem, now } =
    Card.new
        |> Card.title [ Html.h3 [] [ text "Passkeys" ] ]
        |> Card.description [ text "Sign in with your fingerprint, face or screen lock." ]
        |> Card.content []
            [ div [ class "flex flex-col gap-3" ]
                [ div [ class "max-h-48 overflow-y-auto" ]
                    [ case passkeys of
                        Loading ->
                            ul [ class "flex flex-col gap-2", attribute "aria-hidden" "true" ]
                                [ li []
                                    [ Item.new
                                        |> Item.muted
                                        |> Item.icon (Skeleton.view [ class "size-4" ])
                                        |> Item.title [ Skeleton.view [ class "h-[1.375em] w-28 max-w-full" ] ]
                                        |> Item.actions
                                            [ Skeleton.view [ class "h-4 w-16" ]
                                            , Skeleton.view [ class "size-7" ]
                                            ]
                                        |> Item.toHtml [ class "flex-nowrap! [&_[data-slot=item-content]]:min-w-0 [&_[data-slot=item-title]]:w-full" ]
                                    ]
                                ]

                        Loaded [] ->
                            Item.new
                                |> Item.outline
                                |> Item.icon (Phosphor.fingerprint Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                                |> Item.title [ span [ class "flex h-7 items-center" ] [ text "No passkeys yet." ] ]
                                |> Item.toHtml [ class "border-dashed" ]

                        Loaded found ->
                            ul [ class "flex flex-col gap-2" ] (List.map (viewPasskey now confirming) found)

                        NotLoaded message ->
                            p [ class "text-sm text-muted-foreground" ] [ text message ]
                    ]
                , viewProblem ceremony
                , case removalProblem of
                    Just reason ->
                        Field.error [] [ text reason ]

                    Nothing ->
                        text ""
                ]
            ]
        |> Card.footer [ class "flex-col items-start gap-3 sm:flex-row sm:items-center sm:justify-between" ]
            [ p [ class "text-xs text-muted-foreground" ] [ text "You can still log in with an email link." ]
            , if session == Unknown then
                Button.new
                    |> Button.text "Add a passkey"
                    |> Button.startIcon (Phosphor.plus Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                    |> Button.small
                    |> viewButtonSkeleton [ class "max-sm:w-full" ]

              else
                Button.new
                    |> Button.text
                        (if ceremony == Waiting then
                            "Waiting for your device…"

                         else
                            "Add a passkey"
                        )
                    |> Button.startIcon (Phosphor.plus Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                    |> Button.small
                    |> Button.toHtml [ onClick PasskeyRequested, disabled (ceremony == Waiting), class "max-sm:w-full" ]
            ]
        |> Card.toHtml [ class "w-full [--card-spacing:--spacing(5)] sm:[--card-spacing:--spacing(6)]" ]


viewButtonSkeleton : List (Html.Attribute msg) -> Button.Button shape Button.Ready msg -> Html msg
viewButtonSkeleton attrs button =
    div (class "relative inline-flex" :: attribute "aria-hidden" "true" :: attrs)
        [ Button.toHtml [ class "invisible w-full", disabled True ] button
        , Skeleton.view [ class "absolute inset-0 rounded-lg!" ]
        ]


viewPasskey : Maybe Time.Posix -> Maybe CredentialId -> Api.Passkey -> Html Msg
viewPasskey now confirming passkey =
    let
        name =
            Maybe.withDefault "Passkey" passkey.name
    in
    li []
        [ Item.new
            |> Item.muted
            |> Item.icon (Phosphor.fingerprint Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
            |> Item.title
                [ span [ class "truncate", Html.Attributes.title name ]
                    [ text
                        (if confirming == Just passkey.id then
                            "Remove " ++ name ++ "?"

                         else
                            name
                        )
                    ]
                ]
            |> Item.actions
                (if confirming == Just passkey.id then
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

                 else
                    [ case now of
                        Just current ->
                            Html.time
                                [ attribute "datetime" passkey.createdAt
                                , Html.Attributes.title ("Added " ++ passkey.createdAt)
                                , class "whitespace-nowrap text-xs text-muted-foreground"
                                ]
                                [ text (RelativeTime.toString current passkey.createdAt) ]

                        Nothing ->
                            Skeleton.view [ class "h-4 w-16", attribute "aria-hidden" "true" ]
                    , Button.new
                        |> Button.icon (Phosphor.trash Regular |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
                        |> Button.label ("Remove " ++ name)
                        |> Button.ghost
                        |> Button.small
                        |> Button.toHtml [ onClick (PasskeyRemoveRequested passkey.id) ]
                    ]
                )
            |> Item.toHtml [ class "flex-nowrap! [&_[data-slot=item-content]]:min-w-0 [&_[data-slot=item-title]]:w-full" ]
        ]


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
        |> Button.text (submitLabel attempt)
        |> Button.toHtml [ type_ "submit", disabled (attempt == Submitting), class "h-10" ]


submitLabel : Attempt -> String
submitLabel attempt =
    case attempt of
        Submitting ->
            "Sending the link…"

        Editing _ ->
            "Email me a link"


viewEnrolment : Ceremony -> List (Html Msg)
viewEnrolment ceremony =
    [ viewLead "Create your passkey"
        "A passkey signs you in with your fingerprint, face or screen lock, with no email to wait for."
    , div [ class "flex flex-col gap-4" ]
        [ viewProblem ceremony
        , Button.new
            |> Button.text
                (if ceremony == Waiting then
                    "Waiting for your device…"

                 else
                    "Create a passkey"
                )
            |> Button.startIcon (Phosphor.fingerprint Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
            |> Button.toHtml [ onClick PasskeyRequested, disabled (ceremony == Waiting), class "h-10" ]
        , Button.new
            |> Button.text "Not now"
            |> Button.outline
            |> Button.toAnchor [ href (Path.toString Path.Explore), class "h-10" ]
        ]
    ]


viewVerification : Verification -> List (Html Msg)
viewVerification verification =
    case verification of
        Looking _ ->
            [ viewLead "Checking your link" "This takes a moment."
            , div [ class "flex flex-col gap-4", attribute "aria-hidden" "true" ]
                [ viewIdentity
                    (Skeleton.view [ class "size-10 shrink-0 rounded-full!" ])
                    [ Skeleton.view [ class "h-3.5 w-16" ]
                    , Skeleton.view [ class "mt-1.5 h-3.5 w-40" ]
                    ]
                , Skeleton.view [ class "h-10 w-full" ]
                , Skeleton.view [ class "my-0.5 h-4 w-40" ]
                ]
            ]

        Found _ found acceptance ->
            viewFound found acceptance

        Refused reason ->
            [ viewLead "This link didn't work" reason
            , div [ class "flex flex-col gap-4" ]
                [ Button.new
                    |> Button.text "Sign up again"
                    |> Button.toAnchor [ href (Path.toString Path.SignUp), class "h-10" ]
                , Button.new
                    |> Button.text "Log in"
                    |> Button.outline
                    |> Button.toAnchor [ href (Path.toString Path.SignIn), class "h-10" ]
                ]
            ]


viewFound : Api.Verification -> Acceptance -> List (Html Msg)
viewFound found acceptance =
    let
        username =
            Username.toString found.username

        words =
            if found.newAccount then
                { heading = "Create this account?"
                , body = "This creates your account and signs you in."
                , accept = "Create my account"
                , accepting = "Creating your account…"
                }

            else
                { heading = "Log in to this account?"
                , body = "This signs you in on this device."
                , accept = "Log in"
                , accepting = "Logging in…"
                }
    in
    [ viewLead words.heading words.body
    , div [ class "flex flex-col gap-4" ]
        [ viewIdentity
            (Avatar.new
                |> Avatar.fallback [ text (String.toUpper (String.left 2 username)) ]
                |> Avatar.large
                |> Avatar.toHtml [ attribute "aria-hidden" "true" ]
            )
            [ dl [ class "grid text-sm leading-tight" ]
                [ dt [ class "sr-only" ] [ text "Username" ]
                , dd [ class "truncate font-medium" ] [ text username ]
                , dt [ class "sr-only" ] [ text "Email" ]
                , dd [ class "break-all text-muted-foreground" ] [ text (Email.toString found.email) ]
                ]
            ]
        , Button.new
            |> Button.text
                (if acceptance == Accepting then
                    words.accepting

                 else
                    words.accept
                )
            |> Button.toHtml [ onClick VerificationAccepted, disabled (acceptance == Accepting), class "h-10" ]
        , p [ class "text-sm text-muted-foreground" ] [ text "Not you? Close this page." ]
        ]
    ]


viewIdentity : Html Msg -> List (Html Msg) -> Html Msg
viewIdentity avatar details =
    div [ class "flex items-center gap-3 rounded-xl border p-3" ]
        [ avatar
        , div [ class "min-w-0 flex-1" ] details
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
        [ icon Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ]
        , span [ class "sr-only" ] [ text label ]
        ]
        []
