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
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, containing, id, tag, text)
import Test.Http
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


suite : Test
suite =
    describe "Main"
        [ describe "Server card"
            [ test "asks the server for its health on start" <|
                \() ->
                    startAt "/settings/server" defaultFlags
                        |> ProgramTest.expectHttpRequestWasMade "GET" "/api/health"
            , test "names every check while the first report loads" <|
                \() ->
                    startAt "/settings/server" defaultFlags
                        |> ProgramTest.expectViewHas [ text "Database" ]
            , test "shows the report" <|
                \() ->
                    startAt "/settings/server" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/health" passing
                        |> ProgramTest.ensureViewHas [ text "27 ms" ]
                        |> ProgramTest.ensureViewHas [ text "0641607" ]
                        |> ProgramTest.expectViewHas [ text "Region arn1" ]
            , test "shows a failing report, which comes with a 503" <|
                \() ->
                    startAt "/settings/server" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/health" (response 503 failing)
                        |> ProgramTest.ensureViewHas [ class "bg-red-500" ]
                        |> ProgramTest.expectViewHas [ text "Database unreachable" ]
            , test "shows the error the server answers with" <|
                \() ->
                    startAt "/settings/server" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/health" (response 500 """{"error":"Out of order"}""")
                        |> ProgramTest.expectViewHas [ text "Out of order" ]
            , test "says so when the server can't be reached" <|
                \() ->
                    startAt "/settings/server" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/health" Test.Http.networkError
                        |> ProgramTest.expectViewHas [ text "Could not reach the server" ]
            , test "keeps the last report while it refreshes" <|
                \() ->
                    startAt "/settings/server" defaultFlags
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
                        |> ProgramTest.expectViewHas [ text "We sent a link to ada@example.com. It expires in 15 minutes." ]
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
                            [ text "If ada@example.com has an account, we sent it a link. It expires in 15 minutes." ]
            , test "shows why the server refused" <|
                \() ->
                    startAt "/auth/signin" defaultFlags
                        |> ProgramTest.fillIn "email" "Email" "ada@example.com"
                        |> ProgramTest.clickButton "Email me a link"
                        |> ProgramTest.simulateHttpResponse "POST"
                            "/api/auth/signin"
                            (response 429 """{"error":"Too many attempts. Try again later."}""")
                        |> ProgramTest.expectViewHas [ text "Too many attempts. Try again later." ]
            , test "signs in with a passkey: a challenge, the browser, then the dashboard" <|
                \() ->
                    startAt "/auth/signin" defaultFlags
                        |> ProgramTest.clickButton "Sign in with a passkey"
                        |> ProgramTest.simulateHttpOk "POST"
                            "/api/auth/signin/passkey/options"
                            """{"challenge":"Y2hhbGxlbmdl","rpId":"pixo.pics"}"""
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.map3 (\tag challenge rpId -> ( tag, challenge, rpId ))
                                (Decode.field "tag" Decode.string)
                                (Decode.at [ "data", "challenge" ] Decode.string)
                                (Decode.at [ "data", "rpId" ] Decode.string)
                            )
                            (Expect.equal [ ( "GetPasskey", "Y2hhbGxlbmdl", "pixo.pics" ) ])
                        |> ProgramTest.ensureViewHas [ text "Waiting for your device…" ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" passkeyUsed
                        |> ProgramTest.ensureHttpRequest "POST"
                            "/api/auth/signin/passkey"
                            (.body
                                >> Expect.equal
                                    """{"id":"oKGio6SlpqeoqaqrrK2urw","clientDataJSON":"eyJ0eXBlIjo","authenticatorData":"hc4asGtQ","signature":"MEQCIQCA"}"""
                            )
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/signin/passkey" account
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/dashboard")
                        |> ProgramTest.expectViewHas [ text "Welcome, ada" ]
            , test "says so when no passkey is used" <|
                \() ->
                    startAt "/auth/signin" defaultFlags
                        |> ProgramTest.clickButton "Sign in with a passkey"
                        |> ProgramTest.simulateHttpOk "POST"
                            "/api/auth/signin/passkey/options"
                            """{"challenge":"Y2hhbGxlbmdl","rpId":"pixo.pics"}"""
                        |> ProgramTest.simulateIncomingPort "interopToElm" (passkeyFailed "NotAllowedError")
                        |> ProgramTest.ensureViewHas [ text "No passkey was used." ]
                        |> ProgramTest.expectViewHas [ text "Sign in with a passkey" ]
            , test "shows why the server refused the passkey" <|
                \() ->
                    startAt "/auth/signin" defaultFlags
                        |> ProgramTest.clickButton "Sign in with a passkey"
                        |> ProgramTest.simulateHttpOk "POST"
                            "/api/auth/signin/passkey/options"
                            """{"challenge":"Y2hhbGxlbmdl","rpId":"pixo.pics"}"""
                        |> ProgramTest.simulateIncomingPort "interopToElm" passkeyUsed
                        |> ProgramTest.simulateHttpResponse "POST"
                            "/api/auth/signin/passkey"
                            (response 401 """{"error":"This passkey could not be verified. Try again, or sign in with an emailed link."}""")
                        |> ProgramTest.expectViewHas
                            [ text "This passkey could not be verified. Try again, or sign in with an emailed link." ]
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
            , test "doesn't take a failed check for being signed out" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" Test.Http.networkError
                        |> ProgramTest.ensureViewHasNot [ text "Create an account" ]
                        |> ProgramTest.expectViewHas [ text "Can't check who is signed in." ]
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
            , test "stays put and says why when the session can't be checked" <|
                \() ->
                    startAt "/dashboard" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET"
                            "/api/auth/session"
                            (response 503 """{"error":"Not available right now. Try again later."}""")
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/dashboard")
                        |> ProgramTest.expectViewHas [ text "Not available right now. Try again later." ]
            , test "checks the session again on request" <|
                \() ->
                    startAt "/dashboard" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET"
                            "/api/auth/session"
                            (response 503 """{"error":"Not available right now. Try again later."}""")
                        |> ProgramTest.simulateHttpResponse "GET"
                            "/api/auth/passkeys"
                            (response 503 """{"error":"Not available right now. Try again later."}""")
                        |> ProgramTest.clickButton "Try again"
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.ensureViewHas [ text "Welcome, ada" ]
                        |> ProgramTest.expectViewHas [ text "Apple Passwords" ]
            ]
        , describe "Passkeys"
            [ test "lists the account's passkeys on the dashboard" <|
                \() ->
                    startAt "/dashboard" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.ensureViewHas [ text "Apple Passwords" ]
                        |> ProgramTest.expectViewHas [ text "Added 2026-11-10" ]
            , test "calls a passkey just that when nothing says what made it" <|
                \() ->
                    startAt "/dashboard" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.simulateHttpOk "GET"
                            "/api/auth/passkeys"
                            """[{"id":"oKGio6SlpqeoqaqrrK2urw","name":null,"createdAt":"2026-11-10T09:30:00Z"}]"""
                        |> ProgramTest.expectViewHas [ text "Passkey" ]
            , test "removes a passkey once that is confirmed" <|
                \() ->
                    startAt "/dashboard" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.clickButton "Remove Apple Passwords"
                        |> ProgramTest.ensureViewHas [ text "Remove Apple Passwords?" ]
                        |> ProgramTest.ensureHttpRequests "DELETE"
                            "/api/auth/passkeys/oKGio6SlpqeoqaqrrK2urw"
                            (List.length >> Expect.equal 0)
                        |> ProgramTest.clickButton "Remove"
                        |> ProgramTest.simulateHttpOk "DELETE" "/api/auth/passkeys/oKGio6SlpqeoqaqrrK2urw" "{}"
                        |> ProgramTest.expectViewHas [ text "No passkeys yet." ]
            , test "keeps a passkey when its removal is cancelled" <|
                \() ->
                    startAt "/dashboard" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.clickButton "Remove Apple Passwords"
                        |> ProgramTest.clickButton "Cancel"
                        |> ProgramTest.ensureViewHas [ text "Added 2026-11-10" ]
                        |> ProgramTest.expectHttpRequests "DELETE"
                            "/api/auth/passkeys/oKGio6SlpqeoqaqrrK2urw"
                            (List.length >> Expect.equal 0)
            , test "keeps a passkey the server did not remove, and says why" <|
                \() ->
                    startAt "/dashboard" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.clickButton "Remove Apple Passwords"
                        |> ProgramTest.clickButton "Remove"
                        |> ProgramTest.simulateHttpResponse "DELETE"
                            "/api/auth/passkeys/oKGio6SlpqeoqaqrrK2urw"
                            (response 503 """{"error":"Not available right now. Try again later."}""")
                        |> ProgramTest.ensureViewHas [ text "Not available right now. Try again later." ]
                        |> ProgramTest.expectViewHas [ text "Added 2026-11-10" ]
            , test "says so when the account has none" <|
                \() ->
                    startAt "/dashboard" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" "[]"
                        |> ProgramTest.expectViewHas [ text "No passkeys yet." ]
            , test "asks the server for options, the browser for a passkey, then saves it" <|
                \() ->
                    startAt "/dashboard" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" "[]"
                        |> ProgramTest.clickButton "Add a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            passkeyRequest
                            (Expect.equal
                                [ { tag = "CreatePasskey"
                                  , challenge = "Y2hhbGxlbmdl"
                                  , rp = "pixo.pics"
                                  , user = ( "7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11", "ada@example.com", "ada" )
                                  , algorithms = [ -8, -7, -257 ]
                                  , excludeCredentials = [ "b2xkZXI" ]
                                  }
                                ]
                            )
                        |> ProgramTest.simulateIncomingPort "interopToElm" passkeyCreated
                        |> ProgramTest.ensureHttpRequest "POST"
                            "/api/auth/passkeys"
                            (.body
                                >> Expect.equal
                                    """{"clientDataJSON":"eyJ0eXBlIjo","attestationObject":"o2NmbXRkbm9uZQ","transports":["internal","hybrid"]}"""
                            )
                        |> ProgramTest.simulateHttpResponse "POST" "/api/auth/passkeys" (response 201 passkey)
                        |> ProgramTest.ensureViewHasNot [ text "No passkeys yet." ]
                        |> ProgramTest.expectViewHas [ text "Added 2026-11-10" ]
            , test "says so when the passkey is not created" <|
                \() ->
                    startAt "/dashboard" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.clickButton "Add a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.simulateIncomingPort "interopToElm" (passkeyFailed "NotAllowedError")
                        |> ProgramTest.ensureViewHas [ text "The passkey was not created." ]
                        |> ProgramTest.expectHttpRequests "POST" "/api/auth/passkeys" (List.length >> Expect.equal 0)
            , test "says so when the device already has one" <|
                \() ->
                    startAt "/dashboard" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.clickButton "Add a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.simulateIncomingPort "interopToElm" (passkeyFailed "InvalidStateError")
                        |> ProgramTest.expectViewHas [ text "This device already has a passkey for this account." ]
            , test "shows why the server refused the passkey" <|
                \() ->
                    startAt "/dashboard" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.clickButton "Add a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.simulateIncomingPort "interopToElm" passkeyCreated
                        |> ProgramTest.simulateHttpResponse "POST"
                            "/api/auth/passkeys"
                            (response 422 """{"error":"This passkey could not be verified. Try again."}""")
                        |> ProgramTest.expectViewHas [ text "This passkey could not be verified. Try again." ]
            , test "keeps adding a passkey when a removal finishes meanwhile" <|
                \() ->
                    startAt "/dashboard" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.clickButton "Add a passkey"
                        |> ProgramTest.clickButton "Remove Apple Passwords"
                        |> ProgramTest.clickButton "Remove"
                        |> ProgramTest.simulateHttpOk "DELETE" "/api/auth/passkeys/oKGio6SlpqeoqaqrrK2urw" "{}"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.simulateIncomingPort "interopToElm" passkeyCreated
                        |> ProgramTest.expectHttpRequestWasMade "POST" "/api/auth/passkeys"
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
                        |> ProgramTest.ensureViewHas [ text "Your account is ready" ]
                        |> ProgramTest.clickLink "Not now" "/dashboard"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/dashboard")
                        |> ProgramTest.ensureViewHas [ text "Welcome, ada" ]
                        |> ProgramTest.expectViewHas [ text "Signed in as ada@example.com" ]
            , test "offers a passkey to a new account, then opens the dashboard" <|
                \() ->
                    startAt "/auth/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/verify" newAccount
                        |> ProgramTest.clickButton "Create my account"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/verify" account
                        |> ProgramTest.clickButton "Create a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.ensureViewHas [ text "Waiting for your device…" ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" passkeyCreated
                        |> ProgramTest.simulateHttpResponse "POST" "/api/auth/passkeys" (response 201 passkey)
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/dashboard")
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.expectViewHas [ text "Added 2026-11-10" ]
            , test "lets a new account try again when the passkey is not created" <|
                \() ->
                    startAt "/auth/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/verify" newAccount
                        |> ProgramTest.clickButton "Create my account"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/verify" account
                        |> ProgramTest.clickButton "Create a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.simulateIncomingPort "interopToElm" (passkeyFailed "NotAllowedError")
                        |> ProgramTest.ensureViewHas [ text "The passkey was not created." ]
                        |> ProgramTest.clickButton "Create a passkey"
                        |> ProgramTest.expectHttpRequestWasMade "POST" "/api/auth/passkeys/options"
            , test "signs in to an account the email has, straight to the dashboard" <|
                \() ->
                    startAt "/auth/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET"
                            "/api/auth/verify"
                            """{"email":"ada@example.com","username":"ada","newAccount":false}"""
                        |> ProgramTest.ensureViewHas [ text "Sign in to this account?" ]
                        |> ProgramTest.clickButton "Sign in"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/verify" account
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/dashboard")
                        |> ProgramTest.expectViewHas [ text "Welcome, ada" ]
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
            , test "offers the dashboard at an unknown address when signed in" <|
                \() ->
                    startAt "/nowhere" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.clickLink "Dashboard" "/dashboard"
                        |> ProgramTest.expectViewHas [ text "Welcome, ada" ]
            , test "offers to sign in or sign up at an unknown address when signed out" <|
                \() ->
                    startAt "/nowhere" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.ensureViewHas [ text "Create an account" ]
                        |> ProgramTest.clickLink "Sign in" "/auth/signin"
                        |> ProgramTest.expectViewHas [ text "We'll email you a link. No password needed." ]
            , test "offers to check the session again at an unknown address" <|
                \() ->
                    startAt "/nowhere" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" Test.Http.networkError
                        |> ProgramTest.clickButton "Try again"
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.expectViewHas [ text "Dashboard" ]
            ]
        , describe "Theme"
            [ test "follows the system when none is stored" <|
                \() ->
                    start { defaultFlags | systemDark = True, entropy = 42 }
                        |> ProgramTest.expectViewHas [ class "dark" ]
            , test "prefers the stored theme to the system's" <|
                \() ->
                    start { defaultFlags | theme = Just "light", systemDark = True }
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
                    startAt "/settings/server" defaultFlags
                        |> ProgramTest.clickButton "Dark"
                        |> ProgramTest.ensureViewHas [ class "dark" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            localStorageWrite
                            (Expect.equal [ { tag = "WriteToLocalStorage", key = "theme", value = "dark" } ])
            ]
        , describe "Shell"
            [ test "opens Explore at home" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.expectViewHas [ tag "h1", containing [ text "Explore" ] ]
            , test "opens the studio" <|
                \() ->
                    startAt "/studio" defaultFlags
                        |> ProgramTest.expectViewHas [ text "Nothing here yet" ]
            , test "leads home from the logo" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.clickLink "pixo" "/"
                        |> ProgramTest.expectViewHas [ tag "h1", containing [ text "Explore" ] ]
            , test "leads to the library and the settings" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.clickLink "Library" "/library"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/library")
                        |> ProgramTest.clickLink "Settings" "/settings/models"
                        |> ProgramTest.ensureViewHas [ text "Remove downloaded models" ]
                        |> ProgramTest.clickLink "Server" "/settings/server"
                        |> ProgramTest.ensureViewHas [ text "GET /api/health" ]
                        |> ProgramTest.clickLink "Studio" "/studio"
                        |> ProgramTest.expectViewHas [ text "Nothing here yet" ]
            , test "marks the page it is on" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.expectViewHas
                            [ tag "a"
                            , attribute (Html.Attributes.attribute "aria-current" "page")
                            , attribute (Html.Attributes.href "/library")
                            ]
            , test "closes the sheet a phone opened once a page is picked" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.update (Main.SidebarChanged { open = True, openMobile = True })
                        |> ProgramTest.clickLink "Library" "/library"
                        |> ProgramTest.expectModel (.sidebar >> Expect.equal { open = True, openMobile = False })
            ]
        , describe "Explore"
            [ test "opens from the menu and marks it" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.clickLink "Explore" "/"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/")
                        |> ProgramTest.expectViewHas
                            [ tag "a"
                            , attribute (Html.Attributes.attribute "aria-current" "page")
                            , attribute (Html.Attributes.href "/")
                            ]
            , test "invites a visitor who is signed out to sign up" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.ensureViewHas [ text "See what people are making" ]
                        |> ProgramTest.clickLink "Sign up" "/auth/signup"
                        |> ProgramTest.expectViewHas [ text "Create your account" ]
            , test "keeps the banner's place, unseen, until it knows who is there" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.expectViewHas
                            [ tag "section"
                            , class "invisible"
                            , containing [ text "See what people are making" ]
                            ]
            , test "shows no banner to someone signed in" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.expectViewHasNot [ text "See what people are making" ]
            , test "asks nothing of the browser" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [])
            ]
        , describe "Studio"
            [ test "asks the browser whether it can run the model" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "CheckWebGpu" ])
            , test "says so when the browser has no WebGPU" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked False)
                        |> ProgramTest.expectViewHas [ text "This browser can't make pictures" ]
            , test "downloads the model, then makes a picture from the prompt, the size, the steps and the seed" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" "  a bonsai on a mountain "
                        |> ProgramTest.check "random-seed" "Random seed" False
                        |> ProgramTest.fillIn "seed" "Seed" "7"
                        |> ProgramTest.clickButton "Download and generate"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            modelLoad
                            (Expect.equal
                                [ Nothing
                                , Just
                                    { model = "prism-ml/bonsai-image-ternary-4B-mlx-2bit"
                                    , cache = True
                                    , force = False
                                    }
                                ]
                            )
                        |> ProgramTest.ensureViewHas [ text "Loading Bonsai ternary, 2-bit…" ]
                        |> ProgramTest.ensureViewHas [ text "0%" ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (modelProgress "text_encoder" 1775000000 1775000000 False)
                        |> ProgramTest.simulateIncomingPort "interopToElm" (modelProgress "transformer" 599000000 1425271472 False)
                        |> ProgramTest.ensureViewHas [ text "Loading the transformer" ]
                        |> ProgramTest.ensureViewHas [ text "42%" ]
                        |> ProgramTest.ensureView
                            (Query.findAll [ attribute (Html.Attributes.attribute "role" "progressbar") ]
                                >> Query.count (Expect.equal 1)
                            )
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "ModelLoaded")
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            generation
                            (List.filterMap identity
                                >> Expect.equal
                                    [ { prompt = "a bonsai on a mountain"
                                      , width = 512
                                      , height = 512
                                      , numInferenceSteps = 4
                                      , seed = 7
                                      , guidanceScale = 1
                                      , frameBudget = 8
                                      }
                                    ]
                            )
                        |> ProgramTest.ensureViewHas [ text "Step 1 of 4" ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (generationStep 0)
                        |> ProgramTest.ensureViewHas [ text "Step 2 of 4" ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (generationStep 3)
                        |> ProgramTest.ensureViewHas [ text "Finishing…" ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.ensureViewHas [ tag "img", attribute (Html.Attributes.src "blob:https://pixo.pics/1") ]
                        |> ProgramTest.expectViewHas [ text "512 × 512 · 4 steps · seed 7" ]
            , test "remembers the model it downloaded" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Download and generate"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "CheckWebGpu", "LoadModel" ])
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "ModelLoaded")
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.maybe localStorageWrite)
                            (List.filterMap identity
                                >> Expect.equal
                                    [ { tag = "WriteToLocalStorage"
                                      , key = "models"
                                      , value = """["prism-ml/bonsai-image-ternary-4B-mlx-2bit"]"""
                                      }
                                    ]
                            )
            , test "loads a model it remembers without calling it a download" <|
                \() ->
                    start { defaultFlags | models = Just """["prism-ml/bonsai-image-binary-4B-mlx-1bit"]""" }
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.ensureViewHasNot [ text "Download and generate" ]
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            modelLoad
                            (Expect.equal
                                [ Nothing
                                , Just
                                    { model = "prism-ml/bonsai-image-binary-4B-mlx-1bit"
                                    , cache = True
                                    , force = False
                                    }
                                ]
                            )
            , test "makes room for another model before it loads it" <|
                \() ->
                    ready
                        |> ProgramTest.clickButton "Bonsai binary, 1-bit"
                        |> ProgramTest.ensureViewHas
                            [ tag "button", id "weights", containing [ text "Bonsai binary, 1-bit" ] ]
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Download and generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "ModelReleased")
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "ModelLoaded")
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (List.filter ((/=) "WriteToLocalStorage")
                                >> Expect.equal [ "ReleaseModel", "LoadModel", "GenerateImage" ]
                            )
            , test "takes a size made for a social network" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Story"
                        |> ProgramTest.ensureViewHas [ text "576 px" ]
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            generation
                            (List.filterMap (Maybe.map (\made -> ( made.width, made.height )))
                                >> Expect.equal [ ( 576, 1024 ) ]
                            )
            , test "draws a new seed for each picture when the seed is random" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            generation
                            (List.filterMap (Maybe.map .seed)
                                >> (\seeds ->
                                        case seeds of
                                            [ first, second ] ->
                                                Expect.notEqual first second

                                            _ ->
                                                Expect.fail "Expected two pictures."
                                   )
                            )
            , test "asks for a prompt before it starts" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.clickButton "Download and generate"
                        |> ProgramTest.ensureViewHas [ text "Describe the picture first." ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "CheckWebGpu" ])
            , test "says why the model did not load" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Download and generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (pipelineFailed "Error" "HTTP 404 for model_index.json")
                        |> ProgramTest.ensureViewHas [ text "HTTP 404 for model_index.json" ]
                        |> ProgramTest.clickButton "Download and generate"
                        |> ProgramTest.expectViewHasNot [ text "HTTP 404 for model_index.json" ]
            , test "stops a picture when asked, and is ready for the next" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.clickButton "Cancel"
                        |> ProgramTest.ensureViewHas [ text "Stopping…" ]
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "GenerateImage", "CancelPipeline" ])
                        |> ProgramTest.simulateIncomingPort "interopToElm" (pipelineFailed "AbortError" "The operation was aborted.")
                        |> ProgramTest.ensureViewHasNot [ text "The operation was aborted." ]
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "GenerateImage" ])
            , test "keeps the model in memory across pages, and checks the browser only once" <|
                \() ->
                    ready
                        |> ProgramTest.clickLink "Settings" "/settings/models"
                        |> ProgramTest.ensureViewHas [ text "In memory" ]
                        |> ProgramTest.clickLink "Studio" "/studio"
                        |> ProgramTest.ensureViewHasNot [ text "Download and generate" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [])
            , test "saves a picture to the library with everything that made it" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.check "random-seed" "Random seed" False
                        |> ProgramTest.fillIn "seed" "Seed" "7"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.clickButton "Save"
                        |> ProgramTest.ensureViewHas [ text "Saving…" ]
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.maybe pictureSave)
                            (List.filterMap identity
                                >> Expect.equal
                                    [ { source = "blob:https://pixo.pics/1"
                                      , prompt = "a bonsai"
                                      , width = 512
                                      , height = 512
                                      , steps = 4
                                      , seed = 7
                                      , model = "prism-ml/bonsai-image-ternary-4B-mlx-2bit"
                                      }
                                    ]
                            )
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (pictureSaved "blob:https://pixo.pics/1" (stored 1 "blob:https://pixo.pics/2" "a bonsai" 512 512))
                        |> ProgramTest.clickLink "Saved" "/library"
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "LoadLibrary" ])
            , test "keeps a picture's details over it once it is tapped, until the next tap" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.ensureViewHas
                            [ tag "figcaption", containing [ text "Bonsai ternary, 2-bit" ] ]
                        |> ProgramTest.ensureViewHas
                            [ tag "button", attribute (Html.Attributes.attribute "aria-pressed" "false") ]
                        |> ProgramTest.clickButton "Show details"
                        |> ProgramTest.ensureViewHas
                            [ tag "button", attribute (Html.Attributes.attribute "aria-pressed" "true") ]
                        |> ProgramTest.clickButton "Show details"
                        |> ProgramTest.expectViewHas
                            [ tag "button", attribute (Html.Attributes.attribute "aria-pressed" "false") ]
            , test "offers to save again when the picture could not be stored" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.clickButton "Save"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (libraryFailed "The quota has been exceeded.")
                        |> ProgramTest.ensureViewHasNot [ text "Saving…" ]
                        |> ProgramTest.clickButton "Save"
                        |> ProgramTest.expectViewHas [ text "Saving…" ]
            ]
        , describe "Models"
            [ test "lists every model with what a download takes" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.ensureViewHas [ text "3.4 GB download" ]
                        |> ProgramTest.expectViewHas [ text "2.9 GB download" ]
            , test "downloads the one that is asked for, and keeps a copy" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.clickButton "Download Bonsai binary, 1-bit"
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            modelLoad
                            (Expect.equal
                                [ Nothing
                                , Just
                                    { model = "prism-ml/bonsai-image-binary-4B-mlx-1bit"
                                    , cache = True
                                    , force = False
                                    }
                                ]
                            )
            , test "shows how far the part being loaded is" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.clickButton "Download Bonsai ternary, 2-bit"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (modelProgress "transformer" 712635736 1425271472 False)
                        |> ProgramTest.ensureViewHas [ text "Loading the transformer" ]
                        |> ProgramTest.ensureViewHas [ text "50%" ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (modelProgress "vae" 168120878 168120878 True)
                        |> ProgramTest.ensureViewHas [ text "Loading the decoder" ]
                        |> ProgramTest.expectViewHas [ text "100%" ]
            , test "offers no download to a model that has one" <|
                \() ->
                    startAt "/settings/models" { defaultFlags | models = Just """["prism-ml/bonsai-image-ternary-4B-mlx-2bit"]""" }
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.ensureViewHas [ text "Downloaded" ]
                        |> ProgramTest.ensureViewHasNot [ attribute (Html.Attributes.attribute "aria-label" "Download Bonsai ternary, 2-bit") ]
                        |> ProgramTest.expectViewHas [ attribute (Html.Attributes.attribute "aria-label" "Download Bonsai binary, 1-bit") ]
            , test "marks the model it downloaded, and remembers it" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.clickButton "Download Bonsai ternary, 2-bit"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "ModelLoaded")
                        |> ProgramTest.ensureViewHas [ text "In memory" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.maybe localStorageWrite)
                            (List.filterMap (Maybe.map .value)
                                >> Expect.equal [ """["prism-ml/bonsai-image-ternary-4B-mlx-2bit"]""" ]
                            )
            , test "downloads a model again on request" <|
                \() ->
                    startAt "/settings/models" { defaultFlags | models = Just """["prism-ml/bonsai-image-ternary-4B-mlx-2bit"]""" }
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.clickButton "Download Bonsai ternary, 2-bit again"
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            modelLoad
                            (Expect.equal
                                [ Nothing
                                , Just
                                    { model = "prism-ml/bonsai-image-ternary-4B-mlx-2bit"
                                    , cache = True
                                    , force = True
                                    }
                                ]
                            )
            , test "says why the model did not download" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.clickButton "Download Bonsai ternary, 2-bit"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (pipelineFailed "Error" "HTTP 404 for model_index.json")
                        |> ProgramTest.ensureViewHas [ text "HTTP 404 for model_index.json" ]
                        |> ProgramTest.clickButton "Download Bonsai ternary, 2-bit"
                        |> ProgramTest.expectViewHasNot [ text "HTTP 404 for model_index.json" ]
            , test "stops a download when asked, without a complaint" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.clickButton "Download Bonsai ternary, 2-bit"
                        |> ProgramTest.clickButton "Cancel"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "CheckWebGpu", "LoadModel", "CancelPipeline" ])
                        |> ProgramTest.simulateIncomingPort "interopToElm" (pipelineFailed "AbortError" "The operation was aborted.")
                        |> ProgramTest.ensureViewHasNot [ text "The operation was aborted." ]
                        |> ProgramTest.expectViewHasNot [ text "Downloaded" ]
            , test "releases the model, then removes the downloaded copies" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.clickButton "Download Bonsai ternary, 2-bit"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "ModelLoaded")
                        |> ProgramTest.ensureViewHas [ text "Release the model first." ]
                        |> ProgramTest.clickButton "Release"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "ModelReleased")
                        |> ProgramTest.clickButton "Remove downloaded models"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "CheckWebGpu", "LoadModel", "WriteToLocalStorage", "ReleaseModel", "ClearModelCache" ])
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "ModelCacheCleared")
                        |> ProgramTest.ensureViewHas [ text "Downloaded models removed." ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            localStorageWrite
                            (Expect.equal [ { tag = "WriteToLocalStorage", key = "models", value = "[]" } ])
            , test "says so when the browser can't run them" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked False)
                        |> ProgramTest.expectViewHas [ text "They need WebGPU. Try a recent Chrome, Edge or Safari." ]
            ]
        , describe "Library"
            [ test "asks the browser for the saved pictures" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "LoadLibrary" ])
            , test "shows every picture in its own shape" <|
                \() ->
                    library
                        |> ProgramTest.ensureViewHas [ text "3 pictures" ]
                        |> ProgramTest.ensureViewHas [ attribute (Html.Attributes.style "aspect-ratio" "1024 / 576") ]
                        |> ProgramTest.expectViewHas [ tag "img", attribute (Html.Attributes.src "blob:https://pixo.pics/3") ]
            , test "says so when nothing is saved yet" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (libraryLoaded [])
                        |> ProgramTest.ensureViewHas [ text "No pictures yet" ]
                        |> ProgramTest.clickLink "Open the studio" "/studio"
                        |> ProgramTest.expectViewHas [ text "Nothing here yet" ]
            , test "says why the pictures could not be read" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (libraryFailed "The database is blocked.")
                        |> ProgramTest.ensureViewHas [ text "Can't open the library" ]
                        |> ProgramTest.expectViewHas [ text "The database is blocked." ]
            , test "finds pictures by their prompt" <|
                \() ->
                    library
                        |> ProgramTest.fillIn "search" "Search prompts" "sunset light"
                        |> ProgramTest.ensureViewHas [ text "2 of 3 pictures" ]
                        |> ProgramTest.ensureViewHas [ tag "img", attribute (Html.Attributes.src "blob:https://pixo.pics/2") ]
                        |> ProgramTest.expectViewHasNot [ tag "img", attribute (Html.Attributes.src "blob:https://pixo.pics/1") ]
            , test "finds a prompt whatever accents the search is typed with" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded
                                [ stored 1 "blob:https://pixo.pics/1" "the old town of krakow at dawn" 512 512
                                , stored 2 "blob:https://pixo.pics/2" "a bowl of ramen" 512 512
                                ]
                            )
                        |> ProgramTest.fillIn "search" "Search prompts" "Kraków"
                        |> ProgramTest.ensureViewHas [ text "1 of 2 pictures" ]
                        |> ProgramTest.expectViewHas [ tag "img", attribute (Html.Attributes.src "blob:https://pixo.pics/1") ]
            , test "says so when no prompt matches" <|
                \() ->
                    library
                        |> ProgramTest.fillIn "search" "Search prompts" "submarine"
                        |> ProgramTest.expectViewHas [ text "Nothing matches “submarine”" ]
            , test "opens a picture with everything that made it" <|
                \() ->
                    library
                        |> ProgramTest.ensureViewHasNot [ tag "elmcn-dialog", attribute (Html.Attributes.attribute "open" "") ]
                        |> ProgramTest.clickButton "Open a lighthouse on a cliff at sunset"
                        |> ProgramTest.ensureViewHas [ tag "elmcn-dialog", attribute (Html.Attributes.attribute "open" "") ]
                        |> ProgramTest.ensureViewHas [ text "1024 × 576 · 4 steps · seed 7 · Bonsai ternary, 2-bit" ]
                        |> ProgramTest.ensureViewHas
                            [ tag "dialog", attribute (Html.Attributes.attribute "aria-label" "Picture") ]
                        |> ProgramTest.clickButton "Close"
                        |> ProgramTest.expectViewHasNot [ tag "elmcn-dialog", attribute (Html.Attributes.attribute "open" "") ]
            , test "keeps a picture's details over it once it is tapped, until the next tap" <|
                \() ->
                    library
                        |> ProgramTest.clickButton "Open a lighthouse on a cliff at sunset"
                        |> ProgramTest.ensureViewHas
                            [ tag "button", attribute (Html.Attributes.attribute "aria-pressed" "false") ]
                        |> ProgramTest.clickButton "Show details"
                        |> ProgramTest.ensureViewHas
                            [ tag "button", attribute (Html.Attributes.attribute "aria-pressed" "true") ]
                        |> ProgramTest.clickButton "Show details"
                        |> ProgramTest.expectViewHas
                            [ tag "button", attribute (Html.Attributes.attribute "aria-pressed" "false") ]
            , test "deletes the picture that is open, and closes it" <|
                \() ->
                    library
                        |> ProgramTest.clickButton "Open a city skyline at sunset"
                        |> ProgramTest.within
                            (Query.find [ id "picture" ])
                            (ProgramTest.clickButton "Delete")
                        |> ProgramTest.ensureViewHas [ text "Delete this picture?" ]
                        |> ProgramTest.within
                            (Query.find [ id "picture" ])
                            (ProgramTest.clickButton "Delete")
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (Encode.object [ ( "tag", Encode.string "PictureDeleted" ), ( "id", Encode.int 3 ) ])
                        |> ProgramTest.ensureViewHasNot [ tag "elmcn-dialog", attribute (Html.Attributes.attribute "open" "") ]
                        |> ProgramTest.expectViewHas [ text "2 pictures" ]
            , test "deletes a picture once that is confirmed" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ])
                        |> ProgramTest.clickButton "Delete"
                        |> ProgramTest.ensureViewHas [ text "Delete this picture?" ]
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "LoadLibrary" ])
                        |> ProgramTest.clickButton "Delete"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.map3 (\name id url -> ( name, id, url ))
                                (Decode.field "tag" Decode.string)
                                (Decode.at [ "data", "id" ] Decode.int)
                                (Decode.at [ "data", "url" ] Decode.string)
                            )
                            (Expect.equal [ ( "DeletePicture", 1, "blob:https://pixo.pics/1" ) ])
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (Encode.object [ ( "tag", Encode.string "PictureDeleted" ), ( "id", Encode.int 1 ) ])
                        |> ProgramTest.expectViewHas [ text "No pictures yet" ]
            , test "keeps a picture when its deletion is cancelled" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ])
                        |> ProgramTest.clickButton "Delete"
                        |> ProgramTest.clickButton "Cancel"
                        |> ProgramTest.ensureViewHasNot [ text "Delete this picture?" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "LoadLibrary" ])
            , test "copies a picture and opens the network to post it on" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ])
                        |> ProgramTest.clickButton "Bluesky"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.map2 Tuple.pair
                                (Decode.field "tag" Decode.string)
                                (Decode.maybe (Decode.at [ "data", "url" ] Decode.string))
                            )
                            (Expect.equal
                                [ ( "LoadLibrary", Nothing )
                                , ( "CopyPicture", Just "blob:https://pixo.pics/1" )
                                , ( "OpenTab", Just "https://bsky.app/intent/compose?text=a%20red%20fox%20in%20the%20snow" )
                                ]
                            )
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "PictureCopied")
                        |> ProgramTest.ensureViewHas [ text "Picture copied." ]
                        |> ProgramTest.clickButton "Dismiss"
                        |> ProgramTest.expectViewHasNot [ text "Picture copied." ]
            , test "says so when the browser won't hand the picture over" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ])
                        |> ProgramTest.clickButton "Copy picture"
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (Encode.object [ ( "tag", Encode.string "ShareFailed" ), ( "name", Encode.string "NotAllowedError" ) ])
                        |> ProgramTest.expectViewHas [ text "Couldn't share the picture. Download it instead." ]
            , test "offers the device's share sheet where there is one" <|
                \() ->
                    startAt "/library" { defaultFlags | canShare = True }
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ])
                        |> ProgramTest.clickButton "Share…"
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.maybe
                                (Decode.map4 (\name url file words -> { tag = name, url = url, name = file, text = words })
                                    (Decode.field "tag" Decode.string)
                                    (Decode.at [ "data", "url" ] Decode.string)
                                    (Decode.at [ "data", "name" ] Decode.string)
                                    (Decode.at [ "data", "text" ] Decode.string)
                                )
                            )
                            (List.filterMap identity
                                >> Expect.equal
                                    [ { tag = "SharePicture"
                                      , url = "blob:https://pixo.pics/1"
                                      , name = "pixo-7.png"
                                      , text = "a red fox in the snow"
                                      }
                                    ]
                            )
            , test "offers no share sheet where there is none" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ])
                        |> ProgramTest.expectViewHasNot [ text "Share…" ]
            ]
        ]


type alias Flags =
    { theme : Maybe String
    , systemDark : Bool
    , entropy : Int
    , models : Maybe String
    , canShare : Bool
    }


defaultFlags : Flags
defaultFlags =
    { theme = Nothing, systemDark = False, entropy = 42, models = Nothing, canShare = False }


ready : ProgramTest Main.Model Main.Msg Main.Effect
ready =
    start { defaultFlags | models = Just """["prism-ml/bonsai-image-ternary-4B-mlx-2bit"]""" }
        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
        |> ProgramTest.clickLink "Settings" "/settings/models"
        |> ProgramTest.clickButton "Download Bonsai ternary, 2-bit again"
        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "ModelLoaded")
        |> ProgramTest.clickLink "Studio" "/studio"
        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
            (Decode.field "tag" Decode.string)
            (Expect.equal [ "CheckWebGpu", "LoadModel" ])


library : ProgramTest Main.Model Main.Msg Main.Effect
library =
    startAt "/library" defaultFlags
        |> ProgramTest.simulateIncomingPort "interopToElm"
            (libraryLoaded
                [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512
                , stored 2 "blob:https://pixo.pics/2" "a lighthouse on a cliff at sunset" 1024 576
                , stored 3 "blob:https://pixo.pics/3" "a city skyline at sunset" 576 1024
                ]
            )


start : Flags -> ProgramTest Main.Model Main.Msg Main.Effect
start =
    startAt "/studio"


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
                , ( "entropy", Encode.int flags.entropy )
                , ( "models", Maybe.withDefault Encode.null (Maybe.map Encode.string flags.models) )
                , ( "canShare", Encode.bool flags.canShare )
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


type alias PasskeyRequest =
    { tag : String
    , challenge : String
    , rp : String
    , user : ( String, String, String )
    , algorithms : List Int
    , excludeCredentials : List String
    }


passkeyRequest : Decode.Decoder PasskeyRequest
passkeyRequest =
    Decode.map6 PasskeyRequest
        (Decode.field "tag" Decode.string)
        (Decode.at [ "data", "challenge" ] Decode.string)
        (Decode.at [ "data", "rp", "id" ] Decode.string)
        (Decode.at [ "data", "user" ]
            (Decode.map3 (\id name displayName -> ( id, name, displayName ))
                (Decode.field "id" Decode.string)
                (Decode.field "name" Decode.string)
                (Decode.field "displayName" Decode.string)
            )
        )
        (Decode.at [ "data", "algorithms" ] (Decode.list Decode.int))
        (Decode.at [ "data", "excludeCredentials" ] (Decode.list Decode.string))


type alias ModelLoad =
    { model : String
    , cache : Bool
    , force : Bool
    }


modelLoad : Decode.Decoder (Maybe ModelLoad)
modelLoad =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\name ->
                if name == "LoadModel" then
                    Decode.map Just
                        (Decode.field "data"
                            (Decode.map3 ModelLoad
                                (Decode.field "model" Decode.string)
                                (Decode.field "cache" Decode.bool)
                                (Decode.field "force" Decode.bool)
                            )
                        )

                else
                    Decode.succeed Nothing
            )


type alias Generation =
    { prompt : String
    , width : Int
    , height : Int
    , numInferenceSteps : Int
    , seed : Int
    , guidanceScale : Float
    , frameBudget : Int
    }


generation : Decode.Decoder (Maybe Generation)
generation =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\name ->
                if name == "GenerateImage" then
                    Decode.map Just
                        (Decode.field "data"
                            (Decode.map7 Generation
                                (Decode.field "prompt" Decode.string)
                                (Decode.field "width" Decode.int)
                                (Decode.field "height" Decode.int)
                                (Decode.field "numInferenceSteps" Decode.int)
                                (Decode.field "seed" Decode.int)
                                (Decode.field "guidanceScale" Decode.float)
                                (Decode.field "frameBudget" Decode.int)
                            )
                        )

                else
                    Decode.succeed Nothing
            )


type alias PictureSave =
    { source : String
    , prompt : String
    , width : Int
    , height : Int
    , steps : Int
    , seed : Int
    , model : String
    }


pictureSave : Decode.Decoder PictureSave
pictureSave =
    Decode.field "data"
        (Decode.map7 PictureSave
            (Decode.field "source" Decode.string)
            (Decode.field "prompt" Decode.string)
            (Decode.field "width" Decode.int)
            (Decode.field "height" Decode.int)
            (Decode.field "steps" Decode.int)
            (Decode.field "seed" Decode.int)
            (Decode.field "model" Decode.string)
        )


stored : Int -> String -> String -> Int -> Int -> Encode.Value
stored id url prompt width height =
    Encode.object
        [ ( "id", Encode.int id )
        , ( "url", Encode.string url )
        , ( "prompt", Encode.string prompt )
        , ( "width", Encode.int width )
        , ( "height", Encode.int height )
        , ( "steps", Encode.int 4 )
        , ( "seed", Encode.int 7 )
        , ( "model", Encode.string "prism-ml/bonsai-image-ternary-4B-mlx-2bit" )
        ]


libraryLoaded : List Encode.Value -> Encode.Value
libraryLoaded pictures =
    Encode.object
        [ ( "tag", Encode.string "LibraryLoaded" )
        , ( "pictures", Encode.list identity pictures )
        ]


pictureSaved : String -> Encode.Value -> Encode.Value
pictureSaved source picture =
    Encode.object
        [ ( "tag", Encode.string "PictureSaved" )
        , ( "source", Encode.string source )
        , ( "picture", picture )
        ]


libraryFailed : String -> Encode.Value
libraryFailed message =
    Encode.object
        [ ( "tag", Encode.string "LibraryFailed" )
        , ( "message", Encode.string message )
        ]


tagged : String -> Encode.Value
tagged name =
    Encode.object [ ( "tag", Encode.string name ) ]


webGpuChecked : Bool -> Encode.Value
webGpuChecked supported =
    Encode.object
        [ ( "tag", Encode.string "WebGpuChecked" )
        , ( "supported", Encode.bool supported )
        ]


modelProgress : String -> Int -> Int -> Bool -> Encode.Value
modelProgress component loaded total fromCache =
    Encode.object
        [ ( "tag", Encode.string "ModelProgress" )
        , ( "component", Encode.string component )
        , ( "loaded", Encode.int loaded )
        , ( "total", Encode.int total )
        , ( "fromCache", Encode.bool fromCache )
        ]


generationStep : Int -> Encode.Value
generationStep step =
    Encode.object
        [ ( "tag", Encode.string "GenerationStep" )
        , ( "step", Encode.int step )
        ]


imageGenerated : String -> Encode.Value
imageGenerated url =
    Encode.object
        [ ( "tag", Encode.string "ImageGenerated" )
        , ( "url", Encode.string url )
        ]


pipelineFailed : String -> String -> Encode.Value
pipelineFailed name message =
    Encode.object
        [ ( "tag", Encode.string "PipelineFailed" )
        , ( "name", Encode.string name )
        , ( "message", Encode.string message )
        ]


passkeyCreated : Encode.Value
passkeyCreated =
    Encode.object
        [ ( "tag", Encode.string "PasskeyCreated" )
        , ( "clientDataJSON", Encode.string "eyJ0eXBlIjo" )
        , ( "attestationObject", Encode.string "o2NmbXRkbm9uZQ" )
        , ( "transports", Encode.list Encode.string [ "internal", "hybrid" ] )
        ]


passkeyUsed : Encode.Value
passkeyUsed =
    Encode.object
        [ ( "tag", Encode.string "PasskeyUsed" )
        , ( "id", Encode.string "oKGio6SlpqeoqaqrrK2urw" )
        , ( "clientDataJSON", Encode.string "eyJ0eXBlIjo" )
        , ( "authenticatorData", Encode.string "hc4asGtQ" )
        , ( "signature", Encode.string "MEQCIQCA" )
        ]


passkeyFailed : String -> Encode.Value
passkeyFailed name =
    Encode.object
        [ ( "tag", Encode.string "PasskeyFailed" )
        , ( "error", Encode.string name )
        ]


passkey : String
passkey =
    """{"id":"oKGio6SlpqeoqaqrrK2urw","name":"Apple Passwords","createdAt":"2026-11-10T09:30:00Z"}"""


passkeyOptions : String
passkeyOptions =
    """
    {
      "challenge": "Y2hhbGxlbmdl",
      "rp": { "id": "pixo.pics", "name": "pixo" },
      "user": { "id": "7b0c9a52-3d0e-4a53-9a7e-0f6f1f3f5b11", "name": "ada@example.com", "displayName": "ada" },
      "algorithms": [-8, -7, -257],
      "excludeCredentials": ["b2xkZXI"]
    }
    """


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
