module MainTest exposing (suite)

import Expect
import Html.Attributes
import Http
import InteropDefinitions as Interop
import Json.Decode as Decode
import Json.Encode as Encode
import Main
import ProgramTest exposing (ProgramTest)
import SimulatedEffect.Cmd
import SimulatedEffect.Http
import SimulatedEffect.Navigation
import SimulatedEffect.Ports
import Test exposing (Test, describe, test)
import Test.Html.Selector exposing (attribute, class, text)
import Test.Http
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


suite : Test
suite =
    describe "Main"
        [ describe "Server card"
            [ test "asks the server for its health on start" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.expectHttpRequestWasMade "GET" "/api/health"
            , test "names every check while the first report loads" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.expectViewHas [ text "Database" ]
            , test "shows the report" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/health" passing
                        |> ProgramTest.ensureViewHas [ text "27 ms" ]
                        |> ProgramTest.ensureViewHas [ text "0641607" ]
                        |> ProgramTest.expectViewHas [ text "Region arn1" ]
            , test "shows a failing report, which comes with a 503" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/health" (response 503 failing)
                        |> ProgramTest.ensureViewHas [ class "bg-red-500" ]
                        |> ProgramTest.expectViewHas [ text "Database unreachable" ]
            , test "shows the error the server answers with" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/health" (response 500 """{"error":"Out of order"}""")
                        |> ProgramTest.expectViewHas [ text "Out of order" ]
            , test "says so when the server can't be reached" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/health" Test.Http.networkError
                        |> ProgramTest.expectViewHas [ text "Could not reach the server" ]
            , test "keeps the last report while it refreshes" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/health" passing
                        |> ProgramTest.clickButton "Refresh"
                        |> ProgramTest.ensureHttpRequestWasMade "GET" "/api/health"
                        |> ProgramTest.expectViewHas [ text "27 ms" ]
            ]
        , describe "Sign up"
            [ test "is one click away from home" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.clickLink "Create an account" "/auth/signup"
                        |> ProgramTest.expectViewHas [ text "Create your account" ]
            , test "sends the email and the username, then says to check the inbox" <|
                \() ->
                    startAt "/auth/signup" defaultFlags
                        |> ProgramTest.fillIn "email" "Email" "ada@example.com"
                        |> ProgramTest.fillIn "username" "Username" "ada"
                        |> ProgramTest.clickButton "Email me a link"
                        |> ProgramTest.ensureHttpRequest "POST"
                            "/api/auth/signup"
                            (.body >> Expect.equal """{"email":"ada@example.com","username":"ada"}""")
                        |> ProgramTest.simulateHttpResponse "POST" "/api/auth/signup" (response 202 "{}")
                        |> ProgramTest.ensureViewHas [ text "Check your inbox" ]
                        |> ProgramTest.expectViewHas [ text "We sent a link to ada@example.com. It works once and expires in 15 minutes." ]
            , test "shows why the server refused, and keeps what was typed" <|
                \() ->
                    startAt "/auth/signup" defaultFlags
                        |> ProgramTest.fillIn "email" "Email" "ada@example.com"
                        |> ProgramTest.fillIn "username" "Username" "ada"
                        |> ProgramTest.clickButton "Email me a link"
                        |> ProgramTest.simulateHttpResponse "POST"
                            "/api/auth/signup"
                            (response 409 """{"error":"That username is taken."}""")
                        |> ProgramTest.ensureViewHas [ text "That username is taken." ]
                        |> ProgramTest.expectViewHas [ attribute (Html.Attributes.value "ada@example.com") ]
            , test "doesn't ask for the server's health" <|
                \() ->
                    startAt "/auth/signup" defaultFlags
                        |> ProgramTest.expectHttpRequests "GET" "/api/health" (List.length >> Expect.equal 0)
            ]
        , describe "Sign in"
            [ test "sends the email, then says to check the inbox" <|
                \() ->
                    startAt "/auth/signin" defaultFlags
                        |> ProgramTest.fillIn "email" "Email" "ada@example.com"
                        |> ProgramTest.clickButton "Email me a link"
                        |> ProgramTest.ensureHttpRequest "POST"
                            "/api/auth/signin"
                            (.body >> Expect.equal """{"email":"ada@example.com"}""")
                        |> ProgramTest.simulateHttpResponse "POST" "/api/auth/signin" (response 202 "{}")
                        |> ProgramTest.expectViewHas
                            [ text "If ada@example.com has an account, we sent it a link. It works once and expires in 15 minutes." ]
            , test "shows why the server refused" <|
                \() ->
                    startAt "/auth/signin" defaultFlags
                        |> ProgramTest.fillIn "email" "Email" "ada@example.com"
                        |> ProgramTest.clickButton "Email me a link"
                        |> ProgramTest.simulateHttpResponse "POST"
                            "/api/auth/signin"
                            (response 429 """{"error":"Too many attempts. Try again later."}""")
                        |> ProgramTest.expectViewHas [ text "Too many attempts. Try again later." ]
            , test "links to sign-up, and back" <|
                \() ->
                    startAt "/auth/signin" defaultFlags
                        |> ProgramTest.clickLink "Create an account" "/auth/signup"
                        |> ProgramTest.ensureViewHas [ text "Create your account" ]
                        |> ProgramTest.clickLink "Sign in" "/auth/signin"
                        |> ProgramTest.expectViewHas [ text "We'll email you a link. No password needed." ]
            ]
        , describe "Session"
            [ test "asks who is signed in on start" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.expectHttpRequestWasMade "GET" "/api/auth/session"
            , test "offers to sign up or sign in at home when signed out" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.ensureViewHas [ text "Create an account" ]
                        |> ProgramTest.clickLink "Sign in" "/auth/signin"
                        |> ProgramTest.expectBrowserUrl (Expect.equal "https://pixo.pics/auth/signin")
            , test "offers the dashboard at home when signed in" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.ensureViewHasNot [ text "Sign in" ]
                        |> ProgramTest.clickLink "Dashboard" "/dashboard"
                        |> ProgramTest.expectViewHas [ text "Welcome, ada" ]
            ]
        , describe "Dashboard"
            [ test "welcomes the signed-in account" <|
                \() ->
                    startAt "/dashboard" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.ensureViewHas [ text "Welcome, ada" ]
                        |> ProgramTest.expectViewHas [ text "Signed in as ada@example.com" ]
            , test "sends a signed-out visitor to sign in" <|
                \() ->
                    startAt "/dashboard" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/auth/signin")
                        |> ProgramTest.expectViewHas [ text "We'll email you a link. No password needed." ]
            , test "signs out, back to home" <|
                \() ->
                    startAt "/dashboard" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.clickButton "Sign out"
                        |> ProgramTest.simulateHttpOk "DELETE" "/api/auth/session" "{}"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/")
                        |> ProgramTest.ensureViewHas [ text "Create an account" ]
                        |> ProgramTest.clickLink "Sign in" "/auth/signin"
                        |> ProgramTest.expectViewHas [ text "We'll email you a link. No password needed." ]
            ]
        , describe "Verify"
            [ test "asks who the link is for, with the token from the fragment" <|
                \() ->
                    startAt "/auth/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.expectHttpRequest "GET"
                            "/api/auth/verify"
                            (.headers >> Expect.equal [ ( "authorization", "Bearer Zm9vYmFy" ) ])
            , test "shows the username and the email before anything happens" <|
                \() ->
                    startAt "/auth/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/verify" newAccount
                        |> ProgramTest.ensureViewHas [ text "Create this account?" ]
                        |> ProgramTest.ensureViewHas [ text "ada@example.com" ]
                        |> ProgramTest.ensureViewHas [ text "ada" ]
                        |> ProgramTest.expectHttpRequests "POST" "/api/auth/verify" (List.length >> Expect.equal 0)
            , test "uses the link once accepted, and welcomes the account" <|
                \() ->
                    startAt "/auth/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/verify" newAccount
                        |> ProgramTest.clickButton "Create my account"
                        |> ProgramTest.ensureHttpRequest "POST"
                            "/api/auth/verify"
                            (.headers >> Expect.equal [ ( "authorization", "Bearer Zm9vYmFy" ) ])
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/verify" account
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/dashboard")
                        |> ProgramTest.ensureViewHas [ text "Welcome, ada" ]
                        |> ProgramTest.expectViewHas [ text "Signed in as ada@example.com" ]
            , test "offers to sign in when the email has an account" <|
                \() ->
                    startAt "/auth/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET"
                            "/api/auth/verify"
                            """{"email":"ada@example.com","username":"ada","newAccount":false}"""
                        |> ProgramTest.ensureViewHas [ text "Sign in to this account?" ]
                        |> ProgramTest.clickButton "Sign in"
                        |> ProgramTest.expectHttpRequestWasMade "POST" "/api/auth/verify"
            , test "says so when the link is not valid any more" <|
                \() ->
                    startAt "/auth/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET"
                            "/api/auth/verify"
                            (response 404 """{"error":"This link is not valid any more. Sign up again to get a new one."}""")
                        |> ProgramTest.ensureViewHas [ text "This link is not valid any more. Sign up again to get a new one." ]
                        |> ProgramTest.clickLink "Sign up again" "/auth/signup"
                        |> ProgramTest.expectViewHas [ text "Create your account" ]
            , test "says so when accepting fails" <|
                \() ->
                    startAt "/auth/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/verify" newAccount
                        |> ProgramTest.clickButton "Create my account"
                        |> ProgramTest.simulateHttpResponse "POST"
                            "/api/auth/verify"
                            (response 409 """{"error":"That username was taken in the meantime. Sign up again with another."}""")
                        |> ProgramTest.expectViewHas [ text "That username was taken in the meantime. Sign up again with another." ]
            , test "asks nothing without a token" <|
                \() ->
                    startAt "/auth/verify" defaultFlags
                        |> ProgramTest.ensureHttpRequests "GET" "/api/auth/verify" (List.length >> Expect.equal 0)
                        |> ProgramTest.expectViewHas [ text "This link is incomplete. Open the one from your email again." ]
            ]
        , describe "Routing"
            [ test "says so at an unknown address" <|
                \() ->
                    startAt "/nowhere" defaultFlags
                        |> ProgramTest.expectViewHas [ text "There is nothing at this address." ]
            ]
        , describe "Theme"
            [ test "follows the system when none is stored" <|
                \() ->
                    start { defaultFlags | systemDark = True }
                        |> ProgramTest.expectViewHas [ class "dark" ]
            , test "prefers the stored theme to the system's" <|
                \() ->
                    start { theme = Just "light", systemDark = True }
                        |> ProgramTest.expectViewHasNot [ class "dark" ]
            , test "follows the system when it changes" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.ensureViewHasNot [ class "dark" ]
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (Encode.object
                                [ ( "tag", Encode.string "SystemThemeChanged" )
                                , ( "dark", Encode.bool True )
                                ]
                            )
                        |> ProgramTest.expectViewHas [ class "dark" ]
            , test "applies the selected theme and saves it" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.clickButton "Dark"
                        |> ProgramTest.ensureViewHas [ class "dark" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            localStorageWrite
                            (Expect.equal [ { tag = "WriteToLocalStorage", key = "theme", value = "dark" } ])
            ]
        ]


type alias Flags =
    { theme : Maybe String
    , systemDark : Bool
    }


defaultFlags : Flags
defaultFlags =
    { theme = Nothing, systemDark = False }


start : Flags -> ProgramTest Main.Model Main.Msg Main.Effect
start =
    startAt "/"


startAt : String -> Flags -> ProgramTest Main.Model Main.Msg Main.Effect
startAt path flags =
    ProgramTest.createApplication
        { init = \json url () -> Main.init json url
        , update = Main.update
        , view = Main.view
        , onUrlRequest = Main.UrlRequested
        , onUrlChange = Main.UrlChanged
        }
        |> ProgramTest.withBaseUrl ("https://pixo.pics" ++ path)
        |> ProgramTest.withSimulatedEffects simulateEffects
        |> ProgramTest.withSimulatedSubscriptions simulateSubscriptions
        |> ProgramTest.start
            (Encode.object
                [ ( "theme", Maybe.withDefault Encode.null (Maybe.map Encode.string flags.theme) )
                , ( "systemDark", Encode.bool flags.systemDark )
                ]
            )


simulateEffects : Main.Effect -> ProgramTest.SimulatedEffect Main.Msg
simulateEffects effect =
    case effect of
        Main.NoEffect ->
            SimulatedEffect.Cmd.none

        Main.Batch effects ->
            SimulatedEffect.Cmd.batch (List.map simulateEffects effects)

        Main.Request { method, headers, url, body, onResponse } ->
            SimulatedEffect.Http.request
                { method = method
                , headers = List.map (\( name, value ) -> SimulatedEffect.Http.header name value) headers
                , url = url
                , body =
                    case body of
                        Just value ->
                            SimulatedEffect.Http.jsonBody value

                        Nothing ->
                            SimulatedEffect.Http.emptyBody
                , expect = SimulatedEffect.Http.expectStringResponse merge (onResponse >> Ok)
                , timeout = Nothing
                , tracker = Nothing
                }

        Main.SendToJs value ->
            SimulatedEffect.Ports.send "interopFromElm"
                (TsEncode.encoder Interop.interop.fromElm value)

        Main.PushUrl url ->
            SimulatedEffect.Navigation.pushUrl url

        Main.Load url ->
            SimulatedEffect.Navigation.load url


simulateSubscriptions : Main.Model -> ProgramTest.SimulatedSub Main.Msg
simulateSubscriptions _ =
    SimulatedEffect.Ports.subscribe "interopToElm"
        Decode.value
        (Decode.decodeValue (TsDecode.decoder Interop.interop.toElm) >> Main.GotInterop)


merge : Result a a -> a
merge result =
    case result of
        Ok value ->
            value

        Err value ->
            value


response : Int -> String -> Http.Response String
response statusCode body =
    Test.Http.httpResponse { statusCode = statusCode, headers = [], body = body }


localStorageWrite : Decode.Decoder { tag : String, key : String, value : String }
localStorageWrite =
    Decode.map3 (\tag key value -> { tag = tag, key = key, value = value })
        (Decode.field "tag" Decode.string)
        (Decode.at [ "data", "key" ] Decode.string)
        (Decode.at [ "data", "value" ] Decode.string)


account : String
account =
    """{"email":"ada@example.com","username":"ada"}"""


notSignedIn : Http.Response String
notSignedIn =
    response 401 """{"error":"Not signed in."}"""


newAccount : String
newAccount =
    """{"email":"ada@example.com","username":"ada","newAccount":true}"""


passing : String
passing =
    """
    {
      "status": "pass",
      "releaseId": "064160704a6b52143928d03db8e50fcd5dfe0205",
      "region": "arn1",
      "links": {
        "release": "https://github.com/forkaround/pixo.pics/commit/064160704a6b52143928d03db8e50fcd5dfe0205"
      },
      "checks": { "database": { "status": "pass", "latencyMs": 27 } }
    }
    """


failing : String
failing =
    """
    {
      "status": "fail",
      "checks": { "database": { "status": "fail", "output": "Database unreachable" } }
    }
    """
