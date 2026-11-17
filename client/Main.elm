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
import Notice
import Page.Account
import Page.Explore
import Page.General
import Page.Passkey
import Page.Profile
import Page.SignIn
import Page.SignUp
import Page.Verify
import Path
import Phosphor exposing (IconWeight(..))
import Pipeline
import Plain
import Process
import Publish
import Session exposing (Session(..))
import StudioView
import Stylist
import SupportView
import Svg.Attributes
import Tagger
import Task
import Time
import UI.Avatar as Avatar
import UI.Button as Button
import UI.DropdownMenu as DropdownMenu
import UI.Empty as Empty
import UI.Sidebar as Sidebar
import UI.Skeleton as Skeleton
import UI.Tabs as Tabs
import UI.Toast as Toast
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
    , me : Maybe Api.Me
    , pipeline : Pipeline.Model
    , stylist : Stylist.Model
    , library : Library.Model
    , publish : Publish.Model
    , tagger : Tagger.Model
    , sidebar : Sidebar.State
    , sidebarProgress : Float
    , runs : Int
    , turns : Int
    , toaster : Toast.Toaster Msg
    , page : Page
    }


type Page
    = Explore Page.Explore.Model
    | Profile Page.Profile.Model
    | Studio
    | Library
    | Settings Section
    | SignUp Page.SignUp.Model
    | SignIn Page.SignIn.Model
    | Verify Page.Verify.Model
    | Passkey Page.Passkey.Model
    | NotFound


type Section
    = General Page.General.Model
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
        , me = Nothing
        , pipeline = Pipeline.init entropy webGpu (Maybe.withDefault [] (Maybe.map Pipeline.remembered models))
        , stylist = Stylist.init stylist
        , library = Library.init canShare
        , publish = Publish.init
        , tagger = Tagger.init
        , sidebar = sidebar
        , sidebarProgress = sidebarTarget sidebar
        , runs = 0
        , turns = 0
        , toaster = Toast.init
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
            Page.Explore.init
                |> Tuple.mapBoth (\page -> { model | page = Explore page }) (Effect.map GotExplore)

        Path.Profile name ->
            Page.Profile.init name
                |> Tuple.mapBoth (\page -> { model | page = Profile page }) (Effect.map GotProfile)

        Path.Studio ->
            runPipeline { model | page = Studio } (Pipeline.enter model.pipeline)

        Path.Library ->
            runLibrary { model | page = Library } (Library.enter model.library)

        Path.General ->
            Page.General.load
                |> Tuple.mapBoth (\page -> { model | page = Settings (General page) }) (Effect.map GotGeneral)

        Path.Models ->
            runPipeline { model | page = Settings Models } (Pipeline.enter model.pipeline)

        Path.SignUp ->
            ( { model | page = SignUp Page.SignUp.init }, NoEffect )

        Path.SignIn ->
            ( { model | page = SignIn Page.SignIn.init }, NoEffect )

        Path.Account ->
            if model.session == SignedOut then
                ( { model | page = Settings (Account Page.Account.init) }, NoEffect )

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
        | pipeline = { pipeline | notice = Nothing }
      }
    , Batch
        (List.filterMap identity
            [ Maybe.map (Interop.RunPipeline >> SendToJs) command
            , Maybe.map (Notice.map GotPipeline >> Notify) pipeline.notice
            , case command of
                Just (Pipeline.Load _) ->
                    Just (Dismiss Pipeline.toast)

                Just (Pipeline.Generate _) ->
                    Just (Dismiss Pipeline.toast)

                _ ->
                    Nothing
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


runStylist : Model -> ( Stylist.Model, Maybe Stylist.Command, Maybe Stylist.Handover ) -> ( Model, Effect Msg )
runStylist model ( stylist, command, handover ) =
    let
        ( written, effect ) =
            case handover of
                Just Stylist.Took ->
                    runPipeline model (Pipeline.update Pipeline.StylingStarted model.pipeline)

                Just (Stylist.Gave text) ->
                    runPipeline model (Pipeline.update (Pipeline.StylingEnded text) model.pipeline)

                Nothing ->
                    ( model, NoEffect )
    in
    ( { written | stylist = { stylist | notice = Nothing } }
    , Batch
        (List.filterMap identity
            [ Just effect
            , Maybe.map (Interop.RunStylist >> SendToJs) command
            , Maybe.map (Notice.map GotStylist >> Notify) stylist.notice
            , case command of
                Just Stylist.Load ->
                    Just (Dismiss Stylist.toast)

                Just (Stylist.Write _) ->
                    Just (Dismiss Stylist.toast)

                _ ->
                    Nothing
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
    ( { model | library = { library | notice = Nothing } }
    , Batch
        (List.map (Interop.RunLibrary >> SendToJs) commands
            ++ List.filterMap identity [ Maybe.map (Notice.map GotLibrary >> Notify) library.notice ]
        )
    )


publishId : String -> Maybe Int
publishId name =
    if String.startsWith "publish:" name then
        String.toInt (String.dropLeft 8 name)

    else
        Nothing


runTagger : Model -> ( Tagger.Model, Maybe Tagger.Command, Maybe Tagger.Found ) -> ( Model, Effect Msg )
runTagger model ( tagger, command, found ) =
    let
        ( tagged, effect ) =
            case found of
                Just { id, tags } ->
                    runLibrary model (Library.update (Library.Tagged id tags) model.library)

                Nothing ->
                    ( model, NoEffect )
    in
    ( { tagged | tagger = tagger }
    , Batch (List.filterMap identity [ Just effect, Maybe.map (Interop.RunTagger >> SendToJs) command ])
    )


canTag : Model -> Bool
canTag model =
    Pipeline.canPrompt model.pipeline.device && model.stylist.downloaded


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
    | GotSession (Result ApiClient.Error Api.Me)
    | MeRetried
    | BillingPolled Int
    | GotPolled Int (Result ApiClient.Error Api.Me)
    | SessionRetried
    | SessionRechecked
    | GotSignIn Page.SignIn.Msg
    | SignOutClicked
    | GotSignOut (Result ApiClient.Error ())
    | GotPasskey Page.Passkey.Msg
    | GotGeneral Page.General.Msg
    | GotAccount Page.Account.Msg
    | GotExplore Page.Explore.Msg
    | GotProfile Page.Profile.Msg
    | PathSelected Path.Path
    | GotPipeline Pipeline.Msg
    | GotLibrary Library.Msg
    | GotPublish Publish.Msg
    | GotStylist Stylist.Msg
    | SidebarChanged Sidebar.State
    | SidebarAnimated Float
    | HourglassTurned
    | Scrolled
    | GotToast (Toast.Msg Msg)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GotInterop InteropPorts.toElm
        , if model.sidebarProgress == sidebarTarget model.sidebar then
            Sub.none

          else
            Browser.Events.onAnimationFrameDelta SidebarAnimated
        , if isWorking model then
            Time.every 4000 (\_ -> HourglassTurned)

          else
            Sub.none
        , Toast.subscriptions GotToast model.toaster
        ]


sidebarTarget : Sidebar.State -> Float
sidebarTarget sidebar =
    if sidebar.open then
        0

    else
        1


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    let
        ( stepped, effect ) =
            step msg model

        ( followed, following ) =
            follow model.session stepped

        ( asked, backfilled ) =
            backfill followed
    in
    ( counted model asked, Batch [ effect, following, backfilled ] )


currentMe : Model -> Maybe Api.Me
currentMe model =
    case ( model.session, model.me ) of
        ( SignedIn account, Just me ) ->
            if me.account.username == account.username then
                Just me

            else
                Nothing

        _ ->
            Nothing


currentBilling : Model -> Maybe Api.Billing
currentBilling model =
    Maybe.map .billing (currentMe model)


follow : Session -> Model -> ( Model, Effect Msg )
follow before model =
    let
        ( followed, released ) =
            followLibrary model
    in
    case followed.session of
        SignedIn account ->
            if sameAccount before account || currentMe followed /= Nothing then
                ( followed, released )

            else
                ( followed, Batch [ released, Request (ApiClient.request Api.session () GotSession) ] )

        SignedOut ->
            ( { followed | me = Nothing }, released )

        Unknown ->
            ( followed, released )


sameAccount : Session -> Api.Account -> Bool
sameAccount before account =
    case before of
        SignedIn earlier ->
            earlier.username == account.username

        _ ->
            False


followLibrary : Model -> ( Model, Effect Msg )
followLibrary model =
    case model.session of
        SignedIn account ->
            if Library.ownerOf model.library == Just account.username then
                ( model, NoEffect )

            else
                let
                    ( owned, released ) =
                        runLibrary { model | publish = Publish.init } (Library.signedIn account.username model.library)

                    ( entered, loading ) =
                        if model.page == Library then
                            runLibrary owned (Library.enter owned.library)

                        else
                            ( owned, NoEffect )
                in
                ( entered, Batch [ released, loading ] )

        SignedOut ->
            if Library.ownerOf model.library == Nothing then
                ( model, NoEffect )

            else
                runLibrary { model | publish = Publish.init } (Library.signedOut model.library)

        Unknown ->
            ( model, NoEffect )


backfill : Model -> ( Model, Effect Msg )
backfill model =
    case Library.untagged model.library of
        [] ->
            ( model, NoEffect )

        jobs ->
            if canTag model && not (isGenerating model.pipeline.device) then
                runTagger model (Tagger.update (Tagger.Asked jobs) model.tagger)

            else
                ( model, NoEffect )


counted : Model -> Model -> Model
counted before after =
    if isWorking after && not (isWorking before) then
        { after | runs = after.runs + 1 }

    else
        after


step : Msg -> Model -> ( Model, Effect Msg )
step msg model =
    case msg of
        GotInterop (Ok (Interop.SystemThemeChanged systemDark)) ->
            ( { model | systemDark = systemDark }, NoEffect )

        GotInterop (Ok (Interop.PasskeyCreated attestation)) ->
            enrol (Enrolment.created attestation) model

        GotInterop (Ok (Interop.PasskeyUsed assertion)) ->
            step (GotSignIn (Page.SignIn.passkeyUsed assertion)) model

        GotInterop (Ok (Interop.PasskeyFailed name)) ->
            case model.page of
                SignIn _ ->
                    step (GotSignIn (Page.SignIn.passkeyFailed name)) model

                _ ->
                    enrol (Enrolment.failed name) model

        GotInterop (Ok (Interop.CheckoutEvent "checkout.completed")) ->
            ( model
            , Batch
                [ Notify { id = proToast, tone = Notice.Success, title = "Payment received", description = Just "Turning on Pro…", retry = Notice.NoRetry }
                , Sleep 2000 (BillingPolled 0)
                ]
            )

        GotInterop (Ok (Interop.CheckoutEvent "checkout.closed")) ->
            closeCheckout model

        GotInterop (Ok (Interop.CheckoutEvent "checkout.error")) ->
            closeCheckout model
                |> Tuple.mapSecond
                    (\effect ->
                        Batch
                            [ effect
                            , Notify { id = "checkout", tone = Notice.Problem, title = "Couldn't open the checkout", description = Nothing, retry = Notice.NoRetry }
                            ]
                    )

        GotInterop (Ok (Interop.CheckoutEvent _)) ->
            ( model, NoEffect )

        GotInterop (Ok (Interop.PipelineEvent event)) ->
            runPipeline model (Pipeline.update (Pipeline.Happened event) model.pipeline)

        GotInterop (Ok (Interop.LibraryEvent event)) ->
            let
                ( stored, effect ) =
                    runLibrary model (Library.update (Library.Happened event) model.library)
            in
            case ( Library.saved event, canTag stored ) of
                ( Just job, True ) ->
                    runTagger stored (Tagger.update (Tagger.Asked [ job ]) stored.tagger)
                        |> Tuple.mapSecond (\asked -> Batch [ effect, asked ])

                _ ->
                    ( stored, effect )

        GotInterop (Ok (Interop.StylistEvent event)) ->
            runStylist model (Stylist.update (Stylist.Happened event) model.stylist)

        GotInterop (Ok (Interop.TaggerEvent event)) ->
            runTagger model (Tagger.update (Tagger.Happened event) model.tagger)

        GotInterop (Err _) ->
            ( model, NoEffect )

        GotPipeline pipelineMsg ->
            runPipeline model (Pipeline.update pipelineMsg model.pipeline)

        GotLibrary libraryMsg ->
            runLibrary model (Library.update libraryMsg model.library)

        GotPublish publishMsg ->
            let
                ( publishing, effect ) =
                    Publish.update model.library publishMsg model.publish

                ( marked, marking ) =
                    case Publish.outcome publishMsg of
                        Publish.Marked id remote ->
                            runLibrary model (Library.markPublished id remote model.library)

                        Publish.Unchanged ->
                            ( model, NoEffect )
            in
            ( { marked | publish = publishing }
            , Batch [ Effect.map GotPublish effect, marking ]
            )

        GotInterop (Ok (Interop.PictureEncoded result)) ->
            case publishId result.label of
                Just id ->
                    step (GotPublish (Publish.Encoded id { url = result.url, bytes = result.bytes })) model

                Nothing ->
                    ( model, NoEffect )

        GotInterop (Ok (Interop.PictureEncodeFailed result)) ->
            case publishId result.label of
                Just id ->
                    step (GotPublish (Publish.EncodeFailed id result.message)) model

                Nothing ->
                    ( model, NoEffect )

        GotInterop (Ok (Interop.BlobSent result)) ->
            case publishId result.label of
                Just id ->
                    step (GotPublish (Publish.Sent id { status = result.status, body = result.body })) model

                Nothing ->
                    ( model, NoEffect )

        GotInterop (Ok (Interop.BlobFailed result)) ->
            case publishId result.label of
                Just id ->
                    step (GotPublish (Publish.SendFailed id result.message)) model

                Nothing ->
                    ( model, NoEffect )

        GotStylist stylistMsg ->
            if Stylist.needsWebGpu stylistMsg && not (Pipeline.canPrompt model.pipeline.device) then
                ( model, NoEffect )

            else if stylistMsg == Stylist.RemoveClicked && not (Tagger.isAsleep model.tagger) then
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

        GotToast toastMsg ->
            Toast.update GotToast toastMsg model.toaster
                |> Tuple.mapBoth (\toaster -> { model | toaster = toaster }) Toaster

        PathSelected path ->
            ( model, PushUrl (Path.toString path) )

        UrlRequested (Browser.Internal url) ->
            if isExplore model.page && Path.parse url == Path.Explore && url.fragment == Just ExploreView.gallery then
                ( model, ScrollTo ExploreView.gallery )

            else
                ( model, PushUrl (Url.toString url) )

        Scrolled ->
            ( model, NoEffect )

        UrlRequested (Browser.External url) ->
            ( model, Load url )

        UrlChanged url ->
            open url model
                |> Tuple.mapSecond (\effect -> Batch (effect :: List.map Dismiss (pageToasts model.page)))

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

        GotSession (Ok me) ->
            case model.session of
                SignedOut ->
                    ( model, NoEffect )

                SignedIn account ->
                    if account.username == me.account.username then
                        ( { model | session = SignedIn me.account, me = Just me }, Dismiss meToast )

                    else
                        ( model, NoEffect )

                Unknown ->
                    ( { model | session = SignedIn me.account, me = Just me }, Dismiss meToast )

        GotSession (Err error) ->
            case ( model.session, error ) of
                ( SignedIn _, _ ) ->
                    if currentMe model == Nothing then
                        ( model
                        , Effect.failure { id = meToast, title = "Can't load your plan", retry = Just MeRetried } error
                        )

                    else
                        ( model, NoEffect )

                ( _, ApiClient.Unauthorized _ ) ->
                    ( { model | session = SignedOut }
                    , case model.page of
                        Passkey _ ->
                            PushUrl (Path.toString Path.SignIn)

                        _ ->
                            NoEffect
                    )

                _ ->
                    ( model
                    , Effect.failure
                        { id = "session"
                        , title = "Can't check sign-in"
                        , retry =
                            case error of
                                ApiClient.Throttled _ _ ->
                                    Just SessionRechecked

                                _ ->
                                    Just SessionRetried
                        }
                        error
                    )

        MeRetried ->
            ( model, Request (ApiClient.request Api.session () GotSession) )

        BillingPolled attempt ->
            ( model, Request (ApiClient.request Api.session () (GotPolled attempt)) )

        GotPolled attempt result ->
            case model.session of
                SignedIn account ->
                    case result of
                        Ok me ->
                            if me.account.username /= account.username then
                                ( model, NoEffect )

                            else if me.billing.plan == Api.Pro then
                                ( { model | me = Just me }
                                , Notify { id = proToast, tone = Notice.Success, title = "You have Pro", description = Nothing, retry = Notice.NoRetry }
                                )

                            else
                                ( { model | me = Just me }, pollAgain attempt )

                        Err _ ->
                            ( model, pollAgain attempt )

                _ ->
                    ( model, NoEffect )

        SessionRechecked ->
            ( { model | session = Unknown }, Request (ApiClient.request Api.session () GotSession) )

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

        GotGeneral pageMsg ->
            case model.page of
                Settings (General page) ->
                    Page.General.update pageMsg page
                        |> Tuple.mapBoth (\next -> { model | page = Settings (General next) }) (Effect.map GotGeneral)

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

        GotExplore pageMsg ->
            case model.page of
                Explore page ->
                    Page.Explore.update pageMsg page
                        |> Tuple.mapBoth (\next -> { model | page = Explore next }) (Effect.map GotExplore)

                _ ->
                    ( model, NoEffect )

        GotProfile pageMsg ->
            let
                ( believed, library ) =
                    case Page.Profile.outcome pageMsg of
                        Page.Profile.Unpublished id ->
                            runLibrary model (Library.unpublished id model.library)

                        Page.Profile.Unchanged ->
                            ( model, NoEffect )
            in
            case believed.page of
                Profile page ->
                    Page.Profile.update believed.session pageMsg page
                        |> Tuple.mapBoth (\next -> { believed | page = Profile next }) (\effect -> Batch [ Effect.map GotProfile effect, library ])

                _ ->
                    ( believed, library )

        ThemeSelected theme ->
            ( { model | theme = theme }
            , SendToJs
                (Interop.WriteToLocalStorage
                    { key = "theme"
                    , value = Interop.themeToString theme
                    }
                )
            )


closeCheckout : Model -> ( Model, Effect Msg )
closeCheckout model =
    case model.page of
        Settings (Account _) ->
            step (GotAccount Page.Account.checkoutClosed) model

        _ ->
            ( model, NoEffect )


pollWaits : List Int
pollWaits =
    [ 3000, 5000, 8000, 13000, 21000 ]


pollAgain : Int -> Effect Msg
pollAgain attempt =
    case List.head (List.drop attempt pollWaits) of
        Just wait ->
            Sleep (toFloat wait) (BillingPolled (attempt + 1))

        Nothing ->
            Notify
                { id = proToast
                , tone = Notice.Information
                , title = "Pro is on its way"
                , description = Just "Your payment went through. Reload in a minute."
                , retry = Notice.NoRetry
                }


meToast : String
meToast =
    "me"


proToast : String
proToast =
    "pro"


enrol : Enrolment.Msg -> Model -> ( Model, Effect Msg )
enrol msg model =
    case model.page of
        Passkey _ ->
            step (GotPasskey (Page.Passkey.enrolment msg)) model

        Settings (Account _) ->
            step (GotAccount (Page.Account.enrolment msg)) model

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

        Notify notice ->
            Task.perform GotToast (Task.succeed (Toast.add (Notice.toToast notice)))

        Dismiss id ->
            Task.perform GotToast (Task.succeed (Toast.close id))

        Sleep milliseconds msg ->
            Task.perform (\_ -> msg) (Process.sleep milliseconds)

        Toaster cmd ->
            cmd



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
            [ viewBody model
            , if lightboxOpen model then
                text ""

              else
                viewToasts model
            ]
        ]
    }


lightboxOpen : Model -> Bool
lightboxOpen model =
    case model.page of
        Studio ->
            StudioView.lightboxOpen model.pipeline

        Library ->
            LibraryView.lightboxOpen model.library

        _ ->
            False


viewToasts : Model -> Html Msg
viewToasts model =
    Toast.view GotToast model.toaster


title : Page -> String
title page =
    case page of
        Explore _ ->
            "Explore · pixo"

        Profile profile ->
            Username.toString profile.name ++ " · pixo"

        Studio ->
            "Studio · pixo"

        Library ->
            "Library · pixo"

        Settings (General _) ->
            "General · pixo"

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
                [ viewContent
                    [ StudioView.canvas { pipeline = GotPipeline, library = GotLibrary } model.session model.library model.pipeline
                    , StudioView.controls { pipeline = GotPipeline, stylist = GotStylist } model.stylist model.pipeline
                    , StudioView.lightbox { pipeline = GotPipeline, library = GotLibrary } model.session (viewToasts model) model.library model.pipeline
                    ]
                ]

        Library ->
            if model.session == SignedOut then
                viewFrame model "Library" [] [ viewContent [ viewLibraryInvitation ] ]

            else
                viewLibrary model

        Explore page ->
            viewFrame model
                "Explore"
                []
                [ viewContent
                    [ Html.map GotExplore (Page.Explore.view (invitation model.session) page)
                    , Page.Explore.lightbox GotExplore (viewToasts model) page
                    ]
                ]

        Profile page ->
            viewFrame model
                (Username.toString page.name)
                []
                [ viewContent
                    [ Html.map GotProfile (Page.Profile.view model.session page)
                    , Page.Profile.lightbox GotProfile model.session (viewToasts model) page
                    ]
                ]

        Settings section ->
            viewFrame model
                "Settings"
                []
                [ viewAside "md:w-56 md:pt-6"
                    [ Sidebar.group []
                        [ Sidebar.menu [ class "max-md:flex-row max-md:flex-wrap" ]
                            [ viewDestination (isGeneral model.page) Path.General Phosphor.info "General"
                            , viewDestination (section == Models) Path.Models Phosphor.cube "Models"
                            , viewDestination (isAccount model.page) Path.Account Phosphor.userCircle "Account"
                            ]
                        ]
                    ]
                , viewContent
                    [ case section of
                        General general ->
                            viewSettingsPage "General"
                                (text "Privacy and release health.")
                                (Page.General.view general)

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
                                (List.map (Html.map GotAccount) (Page.Account.view model.session (currentBilling model) account))
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


pageToasts : Page -> List String
pageToasts page =
    case page of
        SignUp _ ->
            Page.SignUp.toasts

        SignIn _ ->
            Page.SignIn.toasts

        Verify _ ->
            Page.Verify.toasts

        Passkey _ ->
            Page.Passkey.toasts

        Settings (General _) ->
            Page.General.toasts

        Settings (Account _) ->
            Page.Account.toasts

        Settings Models ->
            []

        Explore _ ->
            Page.Explore.toasts

        Profile _ ->
            Page.Profile.toasts

        Studio ->
            []

        Library ->
            Publish.toasts

        NotFound ->
            []


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
                        { running = isWorking model
                        , run = model.runs
                        , turn = model.turns
                        }
                        [ Svg.Attributes.class "h-4 w-auto max-w-none shrink-0" ]
                    ]
                , span [ class "sr-only" ] [ text "pixo" ]
                ]
            , span [ class "sr-only", attribute "role" "status" ] [ text (String.join ". " (work model)) ]
            ]
        |> Sidebar.content
            [ Sidebar.group []
                [ Sidebar.menu []
                    [ viewDestination (isExplore model.page) Path.Explore Phosphor.kanban "Explore"
                    , viewDestination (model.page == Studio) Path.Studio Phosphor.stack "Studio"
                    , viewDestination (model.page == Library) Path.Library Phosphor.slideshow "Library"
                    , viewDestination (isSettings model.page) Path.General Phosphor.sliders "Settings"
                    ]
                ]
            ]
        |> Sidebar.footer
            [ Sidebar.menu [] [ viewSession model ] ]
        |> Sidebar.rail
        |> Sidebar.page [ class "min-w-0" ] page
        |> Sidebar.toHtml [ class "sidebar-morph bg-background text-foreground" ]


work : Model -> List String
work model =
    List.filterMap identity
        [ if isGenerating model.pipeline.device then
            Just "Generating a picture"

          else
            Nothing
        , case Tagger.pending model.tagger of
            0 ->
                Nothing

            1 ->
                Just "Tagging a picture"

            count ->
                Just ("Tagging " ++ String.fromInt count ++ " pictures")
        ]


isWorking : Model -> Bool
isWorking model =
    not (List.isEmpty (work model))


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


isExplore : Page -> Bool
isExplore page =
    case page of
        Explore _ ->
            True

        _ ->
            False


isGeneral : Page -> Bool
isGeneral page =
    case page of
        Settings (General _) ->
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


viewLibrary : Model -> Html Msg
viewLibrary model =
    viewFrame model
        "Library"
        [ Html.map GotLibrary (LibraryView.search model.library) ]
        [ viewAside "md:w-56 md:pt-6" [ Html.map GotLibrary (LibraryView.filters model.library) ]
        , viewContent [ Html.map GotLibrary (LibraryView.view model.library), LibraryView.lightbox { library = GotLibrary, publish = GotPublish } (Maybe.map .plan (currentBilling model)) model.publish (viewToasts model) model.library ]
        ]


viewLibraryInvitation : Html Msg
viewLibraryInvitation =
    div [ class "flex flex-1 flex-col" ]
        [ Empty.new
            |> Empty.icon (Phosphor.slideshow Bold |> Phosphor.toHtml [ attribute "aria-hidden" "true" ])
            |> Empty.title [ text "Your library" ]
            |> Empty.description [ text "Log in to save pictures and find them later." ]
            |> Empty.content
                [ Button.new
                    |> Button.text "Log in"
                    |> Button.small
                    |> Button.toAnchor [ href (Path.toString Path.SignIn) ]
                , Button.new
                    |> Button.text "Create an account"
                    |> Button.small
                    |> Button.outline
                    |> Button.toAnchor [ href (Path.toString Path.SignUp) ]
                ]
            |> Empty.toHtml []
        ]


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
