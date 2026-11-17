module MainTest exposing (suite)

import Effect exposing (Effect)
import Expect
import Html.Attributes
import Http
import InteropDefinitions as Interop
import Json.Decode as Decode
import Json.Encode as Encode
import Main
import Notice
import Pipeline
import ProgramTest exposing (ProgramTest)
import SimulatedEffect.Cmd
import SimulatedEffect.Http
import SimulatedEffect.Navigation
import SimulatedEffect.Ports
import SimulatedEffect.Process
import SimulatedEffect.Task
import SimulatedEffect.Time
import Stylist
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (attribute, class, containing, id, tag, text)
import Test.Http
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode
import UI.Toast as Toast


suite : Test
suite =
    describe "Main"
        [ describe "Sign up"
            [ test "is in the guest's menu" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.clickButton "Create an account"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/signup")
                        |> ProgramTest.expectViewHas [ id "username" ]
            , test "marks its tab" <|
                \() ->
                    startAt "/signup" defaultFlags
                        |> ProgramTest.expectViewHas
                            [ tag "button"
                            , attribute (Html.Attributes.attribute "aria-selected" "true")
                            , containing [ text "Create an account" ]
                            ]
            , test "sends the email and the username, then says to check the inbox" <|
                \() ->
                    startAt "/signup" defaultFlags
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
                    startAt "/signup" defaultFlags
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
                    startAt "/signup" defaultFlags
                        |> ProgramTest.expectHttpRequests "GET" "/api/health" (List.length >> Expect.equal 0)
            ]
        , describe "Sign in"
            [ test "offers the passkey first, then a link by email" <|
                \() ->
                    startAt "/login" defaultFlags
                        |> ProgramTest.expectView
                            (Query.find [ tag "form" ]
                                >> Query.findAll [ tag "button" ]
                                >> Expect.all
                                    [ Query.first >> Query.has [ text "Log in with a passkey" ]
                                    , Query.index 1 >> Query.has [ text "Email me a link" ]
                                    ]
                            )
            , test "sends the email, then says to check the inbox" <|
                \() ->
                    startAt "/login" defaultFlags
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
                    startAt "/login" defaultFlags
                        |> ProgramTest.fillIn "email" "Email" "ada@example.com"
                        |> ProgramTest.clickButton "Email me a link"
                        |> ProgramTest.simulateHttpResponse "POST"
                            "/api/auth/signin"
                            (response 429 """{"error":"Too many attempts."}""")
                        |> ProgramTest.expectViewHas [ text "Too many attempts." ]
            , test "signs in with a passkey: a challenge, the browser, then home" <|
                \() ->
                    startAt "/login" defaultFlags
                        |> ProgramTest.clickButton "Log in with a passkey"
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
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/")
                        |> ProgramTest.expectViewHas [ tag "button", containing [ text "ada@example.com" ] ]
            , test "believes a passkey's sign-in that is answered once the page is left" <|
                \() ->
                    startAt "/login" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.clickButton "Log in with a passkey"
                        |> ProgramTest.simulateHttpOk "POST"
                            "/api/auth/signin/passkey/options"
                            """{"challenge":"Y2hhbGxlbmdl","rpId":"pixo.pics"}"""
                        |> ProgramTest.simulateIncomingPort "interopToElm" passkeyUsed
                        |> ProgramTest.clickLink "pixo" "/"
                        |> ProgramTest.ensureViewHas [ text "Guest" ]
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/signin/passkey" account
                        |> ProgramTest.expectViewHas [ tag "button", containing [ text "ada@example.com" ] ]
            , test "says so when no passkey is used" <|
                \() ->
                    startAt "/login" defaultFlags
                        |> ProgramTest.clickButton "Log in with a passkey"
                        |> ProgramTest.simulateHttpOk "POST"
                            "/api/auth/signin/passkey/options"
                            """{"challenge":"Y2hhbGxlbmdl","rpId":"pixo.pics"}"""
                        |> ProgramTest.simulateIncomingPort "interopToElm" (passkeyFailed "NotAllowedError")
                        |> ProgramTest.ensureViewHas [ text "No passkey was used." ]
                        |> ProgramTest.expectViewHas [ text "Log in with a passkey" ]
            , test "shows why the server refused the passkey" <|
                \() ->
                    startAt "/login" defaultFlags
                        |> ProgramTest.clickButton "Log in with a passkey"
                        |> ProgramTest.simulateHttpOk "POST"
                            "/api/auth/signin/passkey/options"
                            """{"challenge":"Y2hhbGxlbmdl","rpId":"pixo.pics"}"""
                        |> ProgramTest.simulateIncomingPort "interopToElm" passkeyUsed
                        |> ProgramTest.simulateHttpResponse "POST"
                            "/api/auth/signin/passkey"
                            (response 401 """{"error":"This passkey wasn't recognized."}""")
                        |> ProgramTest.expectViewHas
                            [ text "This passkey wasn't recognized." ]
            , test "sends the link again by itself after a 429" <|
                \() ->
                    startAt "/login" defaultFlags
                        |> ProgramTest.fillIn "email" "Email" "ada@example.com"
                        |> ProgramTest.clickButton "Email me a link"
                        |> ProgramTest.simulateHttpResponse "POST" "/api/auth/signin" (throttled 30)
                        |> ProgramTest.ensureViewHas [ text "Retrying in 30 seconds." ]
                        |> ProgramTest.ensureViewHas [ tag "button", containing [ text "Sending the link…" ], attribute (Html.Attributes.disabled True) ]
                        |> ProgramTest.advanceTime 30000
                        |> ProgramTest.ensureHttpRequest "POST"
                            "/api/auth/signin"
                            (.body >> Expect.equal """{"email":"ada@example.com"}""")
                        |> ProgramTest.simulateHttpResponse "POST" "/api/auth/signin" (response 202 "{}")
                        |> ProgramTest.expectViewHas
                            [ text "If ada@example.com has an account, we sent it a link. It expires in 15 minutes." ]
            , test "offers to send the link again when the server can't be reached" <|
                \() ->
                    startAt "/login" defaultFlags
                        |> ProgramTest.fillIn "email" "Email" "ada@example.com"
                        |> ProgramTest.clickButton "Email me a link"
                        |> ProgramTest.simulateHttpResponse "POST" "/api/auth/signin" Test.Http.networkError
                        |> ProgramTest.ensureViewHas [ text "Network offline" ]
                        |> inToast "Couldn't send the link" (ProgramTest.clickButton "Try again")
                        |> ProgramTest.expectHttpRequestWasMade "POST" "/api/auth/signin"
            , test "closes its toasts when the page is left" <|
                \() ->
                    startAt "/login" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.fillIn "email" "Email" "ada@example.com"
                        |> ProgramTest.clickButton "Email me a link"
                        |> ProgramTest.simulateHttpResponse "POST" "/api/auth/signin" Test.Http.networkError
                        |> ProgramTest.ensureViewHas [ text "Couldn't send the link" ]
                        |> ProgramTest.within (Query.find [ id "auth" ]) (ProgramTest.clickButton "Create an account")
                        |> ProgramTest.expectViewHas
                            [ attribute (Html.Attributes.attribute "data-slot" "toast")
                            , attribute (Html.Attributes.attribute "data-ending-style" "")
                            , containing [ text "Couldn't send the link" ]
                            ]
            , test "starts the passkey over by itself after a 429" <|
                \() ->
                    startAt "/login" defaultFlags
                        |> ProgramTest.clickButton "Log in with a passkey"
                        |> ProgramTest.simulateHttpResponse "POST" "/api/auth/signin/passkey/options" (throttled 10)
                        |> ProgramTest.advanceTime 10000
                        |> ProgramTest.expectHttpRequestWasMade "POST" "/api/auth/signin/passkey/options"
            , test "has a tab to sign up, and one back" <|
                \() ->
                    startAt "/login" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.within (Query.find [ id "auth" ]) (ProgramTest.clickButton "Create an account")
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/signup")
                        |> ProgramTest.ensureViewHas [ id "username" ]
                        |> ProgramTest.within (Query.find [ id "auth" ]) (ProgramTest.clickButton "Log in")
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/login")
                        |> ProgramTest.expectViewHas [ text "Log in with a passkey" ]
            , test "fills the page, with no sidebar, and the logo leads home" <|
                \() ->
                    startAt "/login" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.ensureViewHas [ tag "h1", containing [ text "Log in to your account" ] ]
                        |> ProgramTest.ensureViewHasNot [ tag "a", attribute (Html.Attributes.href "/studio") ]
                        |> ProgramTest.within (Query.find [ id "auth" ]) (ProgramTest.clickButton "Create an account")
                        |> ProgramTest.ensureViewHas [ tag "h1", containing [ text "Create your account" ] ]
                        |> ProgramTest.ensureViewHasNot [ tag "a", attribute (Html.Attributes.href "/studio") ]
                        |> ProgramTest.clickLink "pixo" "/"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/")
                        |> ProgramTest.expectViewHas [ tag "a", attribute (Html.Attributes.href "/studio") ]
            ]
        , describe "Session"
            [ test "asks who is signed in on start" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.expectHttpRequestWasMade "GET" "/api/auth/session"
            , test "offers to sign in, in the guest's menu, when signed out" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.ensureViewHas [ tag "button", containing [ text "Not signed in" ] ]
                        |> ProgramTest.clickButton "Log in"
                        |> ProgramTest.expectBrowserUrl (Expect.equal "https://pixo.pics/login")
            , test "offers the account's menu when signed in, which leads to its settings" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.ensureViewHasNot [ text "Log in" ]
                        |> ProgramTest.ensureViewHasNot [ text "Create an account" ]
                        |> ProgramTest.ensureViewHas [ tag "button", containing [ text "ada@example.com" ] ]
                        |> ProgramTest.clickButton "Account settings"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/settings/account")
                        |> ProgramTest.expectViewHas [ text "Signed in as ada (ada@example.com)" ]
            , test "signs out from the account's menu, back to home" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Sign out"
                        |> ProgramTest.simulateHttpOk "DELETE" "/api/auth/session" "{}"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/")
                        |> ProgramTest.expectViewHas [ text "Create an account" ]
            , test "leaves the ways in out of the top bar, so its tools stay put" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.expectView
                            (Query.find [ tag "header" ] >> Query.hasNot [ tag "a", containing [ text "Log in" ] ])
            , test "has one top bar across the page, with the studio's panel under it" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.ensureView (Query.findAll [ tag "header" ] >> Query.count (Expect.equal 1))
                        |> ProgramTest.expectView
                            (Query.find [ tag "header" ] >> Query.has [ tag "h1", containing [ text "Studio" ] ])
            , test "doesn't take a failed check for being signed out" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" Test.Http.networkError
                        |> ProgramTest.ensureViewHasNot [ text "Create an account" ]
                        |> ProgramTest.ensureViewHas [ text "Can't check sign-in" ]
                        |> ProgramTest.clickButton "Try again"
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.expectViewHas [ tag "button", containing [ text "ada@example.com" ] ]
            , test "tries the session again by itself after a 429" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" (throttled 30)
                        |> ProgramTest.ensureViewHas [ text "Retrying in 30 seconds." ]
                        |> ProgramTest.advanceTime 29999
                        |> ProgramTest.ensureHttpRequests "GET" "/api/auth/session" (List.length >> Expect.equal 0)
                        |> ProgramTest.advanceTime 1
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.expectViewHas [ tag "button", containing [ text "ada@example.com" ] ]
            , test "keeps the account page's skeletons while the session can't be checked" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET"
                            "/api/auth/session"
                            (response 503 """{"error":"Unavailable right now."}""")
                        |> ProgramTest.ensureViewHasNot [ text "Not logged in" ]
                        |> ProgramTest.expectViewHasNot [ text "Manage your account and how you sign in." ]
            ]
        , describe "Account"
            [ test "offers Pro to a Free account" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.ensureViewHas [ text "$5 a month." ]
                        |> ProgramTest.expectViewHas [ tag "button", containing [ text "Upgrade" ] ]
            , test "asks for a checkout and hands its values to the browser" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Upgrade"
                        |> ProgramTest.ensureViewHas [ text "Upgrading…" ]
                        |> ProgramTest.ensureHttpRequest "POST" "/api/billing/checkout" (.body >> Expect.equal "")
                        |> ProgramTest.simulateHttpOk "POST" "/api/billing/checkout" checkout
                        |> ProgramTest.ensureViewHas [ text "Upgrading…" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.map4 (\tag transactionId clientToken environment -> { tag = tag, transactionId = transactionId, clientToken = clientToken, environment = environment })
                                (Decode.field "tag" Decode.string)
                                (Decode.at [ "data", "transactionId" ] Decode.string)
                                (Decode.at [ "data", "clientToken" ] Decode.string)
                                (Decode.at [ "data", "environment" ] Decode.string)
                            )
                            (List.filter (\sent -> sent.tag == "OpenCheckout") >> Expect.equal [ { tag = "OpenCheckout", transactionId = "txn_01", clientToken = "test_token", environment = "sandbox" } ])
            , test "brings Upgrade back when the checkout is closed" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Upgrade"
                        |> ProgramTest.simulateHttpOk "POST" "/api/billing/checkout" checkout
                        |> ProgramTest.simulateIncomingPort "interopToElm" (checkoutEvent "checkout.closed")
                        |> ProgramTest.ensureViewHasNot [ text "Upgrading…" ]
                        |> ProgramTest.expectViewHas [ tag "button", containing [ text "Upgrade" ] ]
            , test "turns Pro on once the poll finds it" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateIncomingPort "interopToElm" (checkoutEvent "checkout.completed")
                        |> ProgramTest.ensureViewHas [ text "Payment received" ]
                        |> ProgramTest.advanceTime 2000
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" mePro
                        |> ProgramTest.ensureViewHas [ text "You have Pro" ]
                        |> ProgramTest.expectViewHas [ tag "button", containing [ text "Manage" ] ]
            , test "waits 3 s before polling again when the webhook has not arrived" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateIncomingPort "interopToElm" (checkoutEvent "checkout.completed")
                        |> ProgramTest.advanceTime 2000
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.advanceTime 2999
                        |> ProgramTest.ensureHttpRequests "GET" "/api/auth/session" (List.length >> Expect.equal 0)
                        |> ProgramTest.advanceTime 1
                        |> ProgramTest.expectHttpRequests "GET" "/api/auth/session" (List.length >> Expect.equal 1)
            , test "says Pro is on its way when the polls run out" <|
                \() ->
                    List.foldl
                        (\wait program ->
                            program
                                |> ProgramTest.advanceTime wait
                                |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        )
                        (startAt "/settings/account" defaultFlags
                            |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                            |> ProgramTest.simulateIncomingPort "interopToElm" (checkoutEvent "checkout.completed")
                        )
                        [ 2000, 3000, 5000, 8000, 13000, 21000 ]
                        |> ProgramTest.expectViewHas [ text "Pro is on its way" ]
            , test "sends one session request after a passkey sign-in" <|
                \() ->
                    startAt "/login" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.clickButton "Log in with a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/signin/passkey/options" """{"challenge":"Y2hhbGxlbmdl","rpId":"pixo.pics"}"""
                        |> ProgramTest.simulateIncomingPort "interopToElm" passkeyUsed
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/signin/passkey" account
                        |> ProgramTest.expectHttpRequests "GET" "/api/auth/session" (List.length >> Expect.equal 1)
            , test "sends no second session request after a page load" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.expectHttpRequests "GET" "/api/auth/session" (List.length >> Expect.equal 0)
            , test "stays signed out when the follow-up answer comes after signing out" <|
                \() ->
                    startAt "/login" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.clickButton "Log in with a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/signin/passkey/options" """{"challenge":"Y2hhbGxlbmdl","rpId":"pixo.pics"}"""
                        |> ProgramTest.simulateIncomingPort "interopToElm" passkeyUsed
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/signin/passkey" account
                        |> ProgramTest.clickButton "Sign out"
                        |> ProgramTest.simulateHttpOk "DELETE" "/api/auth/session" "{}"
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.ensureViewHas [ text "Create an account" ]
                        |> ProgramTest.expectViewHasNot [ tag "button", containing [ text "ada@example.com" ] ]
            , test "offers to try again when the plan can't be loaded after a sign-in" <|
                \() ->
                    startAt "/login" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.clickButton "Log in with a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/signin/passkey/options" """{"challenge":"Y2hhbGxlbmdl","rpId":"pixo.pics"}"""
                        |> ProgramTest.simulateIncomingPort "interopToElm" passkeyUsed
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/signin/passkey" account
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" (response 503 """{"error":"Unavailable right now."}""")
                        |> ProgramTest.ensureViewHas [ text "Can't load your plan" ]
                        |> inToast "Can't load your plan" (ProgramTest.clickButton "Try again")
                        |> ProgramTest.expectHttpRequests "GET" "/api/auth/session" (List.length >> Expect.equal 1)
            , test "opens the customer portal for a Pro account" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" mePro
                        |> ProgramTest.clickButton "Manage"
                        |> ProgramTest.ensureViewHas [ text "Managing…" ]
                        |> ProgramTest.simulateHttpOk "POST" "/api/billing/portal" """{"url":"https://sandbox-customer-portal.paddle.com/x"}"""
                        |> ProgramTest.expectPageChange "https://sandbox-customer-portal.paddle.com/x"
            , test "shows a toast when the checkout can't start" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Upgrade"
                        |> ProgramTest.simulateHttpResponse "POST" "/api/billing/checkout" (response 503 """{"error":"Billing is unavailable right now."}""")
                        |> ProgramTest.expectViewHas [ text "Couldn't start the checkout" ]
            , test "shows when Pro ends" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" meEnding
                        |> ProgramTest.expectViewHas [ text "Ends on 2026-12-17." ]
            , test "shows when Pro renews" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" mePro
                        |> ProgramTest.expectViewHas [ text "Renews on 2026-12-17." ]
            , test "says when the last payment failed" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" mePastDue
                        |> ProgramTest.expectViewHas [ text "Your last payment failed. Update your payment method in Manage." ]
            , test "says nothing about the passkeys of a signed-out visitor" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/passkeys" notSignedIn
                        |> ProgramTest.ensureViewHas [ text "Not logged in" ]
                        |> ProgramTest.expectViewHasNot [ text "Can't load passkeys" ]
            , test "deletes the account only after confirmation and returns home signed out" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Delete account"
                        |> ProgramTest.ensureViewHas [ tag "elmcn-dialog", attribute (Html.Attributes.attribute "open" ""), containing [ attribute (Html.Attributes.attribute "role" "alertdialog") ] ]
                        |> ProgramTest.ensureHttpRequests "DELETE" "/api/auth/account" (List.length >> Expect.equal 0)
                        |> ProgramTest.within (Query.find [ attribute (Html.Attributes.attribute "role" "alertdialog") ]) (ProgramTest.clickButton "Delete")
                        |> ProgramTest.ensureViewHas [ text "Deleting…" ]
                        |> ProgramTest.simulateHttpOk "DELETE" "/api/auth/account" "{}"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/")
                        |> ProgramTest.expectViewHas [ text "Create an account" ]
            , test "signs out when the deletion is answered once the page is left" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Delete account"
                        |> ProgramTest.within (Query.find [ attribute (Html.Attributes.attribute "role" "alertdialog") ]) (ProgramTest.clickButton "Delete")
                        |> ProgramTest.clickLink "Explore" "/"
                        |> ProgramTest.ensureViewHas [ tag "button", containing [ text "ada@example.com" ] ]
                        |> ProgramTest.simulateHttpOk "DELETE" "/api/auth/account" "{}"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/")
                        |> ProgramTest.expectViewHas [ text "Create an account" ]
            , test "cancels account deletion" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Delete account"
                        |> ProgramTest.clickButton "Cancel"
                        |> ProgramTest.ensureViewHasNot [ tag "elmcn-dialog", attribute (Html.Attributes.attribute "id" "delete-account"), attribute (Html.Attributes.attribute "open" "") ]
                        |> ProgramTest.expectViewHas [ text "Signed in as ada (ada@example.com)" ]
            , test "keeps deletion pending when Escape is pressed" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Delete account"
                        |> ProgramTest.within (Query.find [ attribute (Html.Attributes.attribute "role" "alertdialog") ]) (ProgramTest.clickButton "Delete")
                        |> ProgramTest.simulateDomEvent
                            (Query.find [ attribute (Html.Attributes.attribute "role" "alertdialog") ])
                            ( "keydown", Encode.object [ ( "key", Encode.string "Escape" ) ] )
                        |> ProgramTest.ensureViewHas [ tag "elmcn-dialog", attribute (Html.Attributes.attribute "open" "") ]
                        |> ProgramTest.ensureViewHas [ tag "button", containing [ text "Cancel" ], attribute (Html.Attributes.disabled True) ]
                        |> ProgramTest.ensureViewHas [ tag "button", containing [ text "Deleting…" ], attribute (Html.Attributes.disabled True) ]
                        |> ProgramTest.expectHttpRequests "DELETE" "/api/auth/account" (List.length >> Expect.equal 1)
            , test "keeps the account and allows retry when deletion fails" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Delete account"
                        |> ProgramTest.within (Query.find [ attribute (Html.Attributes.attribute "role" "alertdialog") ]) (ProgramTest.clickButton "Delete")
                        |> ProgramTest.simulateHttpResponse "DELETE"
                            "/api/auth/account"
                            (Test.Http.httpResponse
                                { statusCode = 503, headers = [], body = "{\"error\":\"Please try again.\"}" }
                            )
                        |> ProgramTest.ensureViewHas [ text "Please try again." ]
                        |> ProgramTest.ensureViewHasNot [ tag "elmcn-dialog", attribute (Html.Attributes.attribute "id" "delete-account"), attribute (Html.Attributes.attribute "open" "") ]
                        |> ProgramTest.ensureViewHas [ text "Signed in as ada (ada@example.com)" ]
                        |> inToast "Couldn't delete account" (ProgramTest.clickButton "Try again")
                        |> ProgramTest.within (Query.find [ id "delete-account" ]) (ProgramTest.clickButton "Delete")
                        |> ProgramTest.simulateHttpOk "DELETE" "/api/auth/account" "{}"
                        |> ProgramTest.expectBrowserUrl (Expect.equal "https://pixo.pics/")
            , test "deletes the account by itself once a 429's wait is over" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Delete account"
                        |> ProgramTest.within (Query.find [ id "delete-account" ]) (ProgramTest.clickButton "Delete")
                        |> ProgramTest.simulateHttpResponse "DELETE" "/api/auth/account" (throttled 20)
                        |> ProgramTest.ensureViewHasNot [ tag "elmcn-dialog", attribute (Html.Attributes.attribute "id" "delete-account"), attribute (Html.Attributes.attribute "open" "") ]
                        |> ProgramTest.ensureViewHas [ tag "button", containing [ text "Deleting account…" ], attribute (Html.Attributes.disabled True) ]
                        |> ProgramTest.advanceTime 20000
                        |> ProgramTest.simulateHttpOk "DELETE" "/api/auth/account" "{}"
                        |> ProgramTest.expectBrowserUrl (Expect.equal "https://pixo.pics/")
            , test "re-checks only the session after a 429" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" (throttled 2)
                        |> ProgramTest.advanceTime 2000
                        |> ProgramTest.ensureHttpRequestWasMade "GET" "/api/auth/session"
                        |> ProgramTest.expectHttpRequests "GET" "/api/auth/passkeys" (List.length >> Expect.equal 1)
            , test "returns to login when the session expired before deletion" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Delete account"
                        |> ProgramTest.within (Query.find [ attribute (Html.Attributes.attribute "role" "alertdialog") ]) (ProgramTest.clickButton "Delete")
                        |> ProgramTest.simulateHttpResponse "DELETE" "/api/auth/account" notSignedIn
                        |> ProgramTest.expectBrowserUrl (Expect.equal "https://pixo.pics/login")
            , test "shows the signed-in account in the subtitle without a welcome card" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.ensureViewHas [ text "Signed in as ada (ada@example.com)" ]
                        |> ProgramTest.ensureViewHasNot [ text "Welcome, ada" ]
                        |> ProgramTest.expectViewHasNot [ text "Signed in on this browser" ]
            , test "asks a signed-out visitor to log in" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/settings/account")
                        |> ProgramTest.ensureViewHas [ text "Not logged in" ]
                        |> ProgramTest.clickLink "Log in" "/login"
                        |> ProgramTest.expectViewHas [ text "Use your passkey, or get a link by email." ]
            , test "is listed in the settings" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickLink "Account" "/settings/account"
                        |> ProgramTest.expectViewHas [ text "Signed in as ada (ada@example.com)" ]
            , test "is listed in the settings for a visitor who is signed out" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.clickLink "Account" "/settings/account"
                        |> ProgramTest.expectViewHas [ text "Not logged in" ]
            , test "signs out, back to home" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.within (Query.find [ tag "section" ]) (ProgramTest.clickButton "Sign out")
                        |> ProgramTest.simulateHttpOk "DELETE" "/api/auth/session" "{}"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/")
                        |> ProgramTest.ensureViewHas [ text "Create an account" ]
                        |> ProgramTest.clickButton "Log in"
                        |> ProgramTest.expectViewHas [ text "Use your passkey, or get a link by email." ]
            , test "stays put and says why when the session can't be checked" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET"
                            "/api/auth/session"
                            (response 503 """{"error":"Unavailable right now."}""")
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/settings/account")
                        |> ProgramTest.expectViewHas [ text "Unavailable right now." ]
            , test "checks the session again on request" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET"
                            "/api/auth/session"
                            (response 503 """{"error":"Unavailable right now."}""")
                        |> ProgramTest.simulateHttpResponse "GET"
                            "/api/auth/passkeys"
                            (response 503 """{"error":"Unavailable right now."}""")
                        |> inToast "Can't check sign-in" (ProgramTest.clickButton "Try again")
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.ensureViewHas [ text "Signed in as ada (ada@example.com)" ]
                        |> ProgramTest.expectViewHas [ text "Apple Passwords" ]
            ]
        , describe "Passkeys"
            [ test "keeps the skeleton and loads the passkeys again on request" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateHttpResponse "GET"
                            "/api/auth/passkeys"
                            (response 503 """{"error":"Please try again."}""")
                        |> ProgramTest.ensureViewHas [ text "Can't load passkeys" ]
                        |> ProgramTest.ensureViewHasNot [ text "No passkeys yet." ]
                        |> inToast "Can't load passkeys" (ProgramTest.clickButton "Try again")
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.expectViewHas [ text "Apple Passwords" ]
            , test "formats the added time once when the passkeys are shown" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.advanceTime 120000
                        |> ProgramTest.simulateHttpOk "GET"
                            "/api/auth/passkeys"
                            """[{"id":"oKGio6SlpqeoqaqrrK2urw","name":"Apple Passwords","createdAt":"1970-01-01T00:00:00Z"}]"""
                        |> ProgramTest.ensureViewHas [ text "2 minutes ago" ]
                        |> ProgramTest.advanceTime 60000
                        |> ProgramTest.expectViewHas [ text "2 minutes ago" ]
            , test "lists the account's passkeys" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.ensureViewHas [ text "Apple Passwords" ]
                        |> ProgramTest.expectViewHas [ tag "time", attribute (Html.Attributes.attribute "datetime" "2026-11-10T09:30:00Z") ]
            , test "calls a passkey just that when nothing says what made it" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateHttpOk "GET"
                            "/api/auth/passkeys"
                            """[{"id":"oKGio6SlpqeoqaqrrK2urw","name":null,"createdAt":"2026-11-10T09:30:00Z"}]"""
                        |> ProgramTest.expectViewHas [ text "Passkey" ]
            , test "removes a passkey once that is confirmed" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
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
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.clickButton "Remove Apple Passwords"
                        |> ProgramTest.within (Query.find [ attribute (Html.Attributes.attribute "data-slot" "item"), containing [ text "Remove Apple Passwords?" ] ]) (ProgramTest.clickButton "Cancel")
                        |> ProgramTest.ensureViewHas [ tag "time", attribute (Html.Attributes.attribute "datetime" "2026-11-10T09:30:00Z") ]
                        |> ProgramTest.expectHttpRequests "DELETE"
                            "/api/auth/passkeys/oKGio6SlpqeoqaqrrK2urw"
                            (List.length >> Expect.equal 0)
            , test "keeps a passkey the server did not remove, and says why" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.clickButton "Remove Apple Passwords"
                        |> ProgramTest.clickButton "Remove"
                        |> ProgramTest.simulateHttpResponse "DELETE"
                            "/api/auth/passkeys/oKGio6SlpqeoqaqrrK2urw"
                            (response 503 """{"error":"Unavailable right now."}""")
                        |> ProgramTest.ensureViewHas [ text "Unavailable right now." ]
                        |> ProgramTest.expectViewHas [ tag "time", attribute (Html.Attributes.attribute "datetime" "2026-11-10T09:30:00Z") ]
            , test "says so when the account has none" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" "[]"
                        |> ProgramTest.expectViewHas [ text "No passkeys yet." ]
            , test "asks the server for options, the browser for a passkey, then saves it" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
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
                        |> ProgramTest.expectViewHas [ tag "time", attribute (Html.Attributes.attribute "datetime" "2026-11-10T09:30:00Z") ]
            , test "says so when the passkey is not created" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Add a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.simulateIncomingPort "interopToElm" (passkeyFailed "NotAllowedError")
                        |> ProgramTest.ensureViewHas [ text "No passkey was created." ]
                        |> ProgramTest.expectHttpRequests "POST" "/api/auth/passkeys" (List.length >> Expect.equal 0)
            , test "says so when the device already has one" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Add a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.simulateIncomingPort "interopToElm" (passkeyFailed "InvalidStateError")
                        |> ProgramTest.expectViewHas [ text "This device already has one." ]
            , test "shows why the server refused the passkey" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Add a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.simulateIncomingPort "interopToElm" passkeyCreated
                        |> ProgramTest.simulateHttpResponse "POST"
                            "/api/auth/passkeys"
                            (response 422 """{"error":"This passkey couldn't be verified."}""")
                        |> ProgramTest.expectViewHas [ text "This passkey couldn't be verified." ]
            , test "keeps adding a passkey when a removal finishes meanwhile" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
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
                    startAt "/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.expectHttpRequest "GET"
                            "/api/auth/verify"
                            (.headers >> Expect.equal [ ( "authorization", "Bearer Zm9vYmFy" ) ])
            , test "shows the username and the email before anything happens" <|
                \() ->
                    startAt "/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/verify" newAccount
                        |> ProgramTest.ensureViewHas [ text "Create this account?" ]
                        |> ProgramTest.ensureViewHas [ text "ada@example.com" ]
                        |> ProgramTest.ensureViewHas [ text "ada" ]
                        |> ProgramTest.expectHttpRequests "POST" "/api/auth/verify" (List.length >> Expect.equal 0)
            , test "leads a new account to a page of its own for a passkey, which can wait" <|
                \() ->
                    startAt "/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/verify" newAccount
                        |> ProgramTest.clickButton "Create my account"
                        |> ProgramTest.ensureHttpRequest "POST"
                            "/api/auth/verify"
                            (.headers >> Expect.equal [ ( "authorization", "Bearer Zm9vYmFy" ) ])
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/verify" account
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" "[]"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/passkey")
                        |> ProgramTest.ensureViewHas [ tag "h1", containing [ text "Create your passkey" ] ]
                        |> ProgramTest.ensureViewHasNot [ tag "a", attribute (Html.Attributes.href "/studio") ]
                        |> ProgramTest.clickLink "Not now" "/"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/")
                        |> ProgramTest.expectViewHas [ tag "button", containing [ text "ada@example.com" ] ]
            , test "creates a new account's passkey, then leads home" <|
                \() ->
                    startAt "/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/verify" newAccount
                        |> ProgramTest.clickButton "Create my account"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/verify" account
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" "[]"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/passkey")
                        |> ProgramTest.clickButton "Create a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.ensureViewHas [ text "Waiting for your device…" ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" passkeyCreated
                        |> ProgramTest.simulateHttpResponse "POST" "/api/auth/passkeys" (response 201 passkey)
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/")
                        |> ProgramTest.clickButton "Account settings"
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.expectViewHas [ tag "time", attribute (Html.Attributes.attribute "datetime" "2026-11-10T09:30:00Z") ]
            , test "lets a new account try again when the passkey is not created" <|
                \() ->
                    startAt "/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/verify" newAccount
                        |> ProgramTest.clickButton "Create my account"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/verify" account
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" "[]"
                        |> ProgramTest.clickButton "Create a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.simulateIncomingPort "interopToElm" (passkeyFailed "NotAllowedError")
                        |> ProgramTest.ensureViewHas [ text "No passkey was created." ]
                        |> ProgramTest.clickButton "Create a passkey"
                        |> ProgramTest.expectHttpRequestWasMade "POST" "/api/auth/passkeys/options"
            , test "sends a signed-out visitor at the passkey page to log in" <|
                \() ->
                    startAt "/passkey" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/login")
                        |> ProgramTest.expectViewHas [ text "Log in with a passkey" ]
            , test "lets someone signed in create a passkey at its page" <|
                \() ->
                    startAt "/passkey" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Create a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.simulateIncomingPort "interopToElm" passkeyCreated
                        |> ProgramTest.simulateHttpResponse "POST" "/api/auth/passkeys" (response 201 passkey)
                        |> ProgramTest.expectBrowserUrl (Expect.equal "https://pixo.pics/")
            , test "asks the browser for no passkey once its page is left" <|
                \() ->
                    startAt "/passkey" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Create a passkey"
                        |> ProgramTest.clickLink "Not now" "/"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [])
            , test "signs in to an account that has a passkey, straight home" <|
                \() ->
                    startAt "/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/verify" oldAccount
                        |> ProgramTest.ensureViewHas [ text "Log in to this account?" ]
                        |> ProgramTest.clickButton "Log in"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/verify" account
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/verify#Zm9vYmFy")
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/")
                        |> ProgramTest.expectViewHas [ tag "button", containing [ text "ada@example.com" ] ]
            , test "believes a link's sign-in that is answered once the page is left" <|
                \() ->
                    startAt "/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/verify" oldAccount
                        |> ProgramTest.clickButton "Log in"
                        |> ProgramTest.clickLink "pixo" "/"
                        |> ProgramTest.ensureViewHas [ text "Guest" ]
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/verify" account
                        |> ProgramTest.expectViewHas [ tag "button", containing [ text "ada@example.com" ] ]
            , test "asks an account that still has no passkey for one each time it signs in by link" <|
                \() ->
                    startAt "/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/verify" oldAccount
                        |> ProgramTest.clickButton "Log in"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/verify" account
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" "[]"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/passkey")
                        |> ProgramTest.expectViewHas [ tag "h1", containing [ text "Create your passkey" ] ]
            , test "leads home when it can't tell whether the account has a passkey" <|
                \() ->
                    startAt "/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/verify" oldAccount
                        |> ProgramTest.clickButton "Log in"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/verify" account
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/passkeys" Test.Http.networkError
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/")
                        |> ProgramTest.expectViewHas [ tag "button", containing [ text "ada@example.com" ] ]
            , test "keeps checking the link and asks again on request" <|
                \() ->
                    startAt "/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/verify" Test.Http.networkError
                        |> ProgramTest.ensureViewHas [ text "Checking your link" ]
                        |> ProgramTest.ensureViewHasNot [ text "This link didn't work" ]
                        |> inToast "Can't check your link" (ProgramTest.clickButton "Try again")
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/verify" newAccount
                        |> ProgramTest.expectViewHas [ text "Create this account?" ]
            , test "says so when the link is not valid any more" <|
                \() ->
                    startAt "/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET"
                            "/api/auth/verify"
                            (response 404 """{"error":"This link is not valid any more. Sign up again to get a new one."}""")
                        |> ProgramTest.ensureViewHas [ text "This link is not valid any more. Sign up again to get a new one." ]
                        |> ProgramTest.clickLink "Sign up again" "/signup"
                        |> ProgramTest.expectViewHas [ id "username" ]
            , test "offers to log in as well when the link is not valid any more" <|
                \() ->
                    startAt "/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET"
                            "/api/auth/verify"
                            (response 404 """{"error":"This link is not valid any more. Sign up again to get a new one."}""")
                        |> ProgramTest.clickLink "Log in" "/login"
                        |> ProgramTest.expectViewHas [ text "Log in with a passkey" ]
            , test "says so when accepting fails" <|
                \() ->
                    startAt "/verify#Zm9vYmFy" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/verify" newAccount
                        |> ProgramTest.clickButton "Create my account"
                        |> ProgramTest.simulateHttpResponse "POST"
                            "/api/auth/verify"
                            (response 409 """{"error":"That username was taken in the meantime. Sign up again with another."}""")
                        |> ProgramTest.expectViewHas [ text "That username was taken in the meantime. Sign up again with another." ]
            , test "asks nothing without a token" <|
                \() ->
                    startAt "/verify" defaultFlags
                        |> ProgramTest.ensureHttpRequests "GET" "/api/auth/verify" (List.length >> Expect.equal 0)
                        |> ProgramTest.expectViewHas [ text "This link is incomplete. Open the one from your email again." ]
            ]
        , describe "Routing"
            [ test "says so at an unknown address" <|
                \() ->
                    startAt "/nowhere/at/all" defaultFlags
                        |> ProgramTest.expectViewHas [ text "There is nothing at this address." ]
            , test "offers the account at an unknown address when signed in" <|
                \() ->
                    startAt "/nowhere/at/all" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickLink "Account" "/settings/account"
                        |> ProgramTest.expectViewHas [ text "Signed in as ada (ada@example.com)" ]
            , test "offers to sign in or sign up at an unknown address when signed out" <|
                \() ->
                    startAt "/nowhere/at/all" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.ensureViewHas [ text "Create an account" ]
                        |> ProgramTest.clickLink "Log in" "/login"
                        |> ProgramTest.expectViewHas [ text "Use your passkey, or get a link by email." ]
            , test "offers to check the session again at an unknown address" <|
                \() ->
                    startAt "/nowhere/at/all" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" Test.Http.networkError
                        |> ProgramTest.clickButton "Try again"
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.expectViewHas [ text "Account" ]
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
                    startAt "/" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.clickButton "Dark"
                        |> ProgramTest.ensureViewHas [ class "dark" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            localStorageWrite
                            (Expect.equal [ { tag = "WriteToLocalStorage", key = "theme", value = "dark" } ])
            , test "is picked in the same menu by someone signed in" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickButton "Dark"
                        |> ProgramTest.ensureViewHas
                            [ tag "button"
                            , attribute (Html.Attributes.attribute "role" "menuitemradio")
                            , attribute (Html.Attributes.attribute "aria-checked" "true")
                            , containing [ text "Dark" ]
                            ]
                        |> ProgramTest.ensureViewHas [ class "dark" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            localStorageWrite
                            (Expect.equal [ { tag = "WriteToLocalStorage", key = "theme", value = "dark" } ])
            , test "is in the guest's menu for a visitor, and not while the session is unknown" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.ensureViewHasNot [ text "Dark" ]
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.expectViewHas [ text "Dark" ]
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
                        |> ProgramTest.clickLink "Settings" "/settings/general"
                        |> ProgramTest.clickLink "Models" "/settings/models"
                        |> ProgramTest.ensureViewHas [ text "Remove downloaded models" ]
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
            , test "collapses over 200 ms and stops at the X" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.update (Main.SidebarChanged { open = False, openMobile = False })
                        |> ProgramTest.update (Main.SidebarAnimated 100)
                        |> ProgramTest.ensureViewHas [ attribute (Html.Attributes.attribute "style" "--sidebar-progress: 0.5") ]
                        |> ProgramTest.update (Main.SidebarAnimated 100)
                        |> ProgramTest.ensureViewHas [ attribute (Html.Attributes.attribute "style" "--sidebar-progress: 1") ]
                        |> ProgramTest.update (Main.SidebarAnimated 1000)
                        |> ProgramTest.expectModel (.sidebarProgress >> Expect.equal 1)
            , test "reverses from the current position without restarting the morph" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.update (Main.SidebarChanged { open = False, openMobile = False })
                        |> ProgramTest.update (Main.SidebarAnimated 80)
                        |> ProgramTest.update (Main.SidebarChanged { open = True, openMobile = False })
                        |> ProgramTest.ensureViewHas [ attribute (Html.Attributes.attribute "style" "--sidebar-progress: 0.4") ]
                        |> ProgramTest.update (Main.SidebarAnimated 40)
                        |> ProgramTest.ensureViewHas [ attribute (Html.Attributes.attribute "style" "--sidebar-progress: 0.2") ]
                        |> ProgramTest.update (Main.SidebarAnimated 1000)
                        |> ProgramTest.expectModel (.sidebarProgress >> Expect.equal 0)
            , test "saves whether it is collapsed" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.update (Main.SidebarChanged { open = False, openMobile = False })
                        |> ProgramTest.update (Main.SidebarChanged { open = False, openMobile = True })
                        |> ProgramTest.update (Main.SidebarChanged { open = True, openMobile = True })
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            localStorageWrite
                            (Expect.equal
                                [ { tag = "WriteToLocalStorage", key = "sidebar", value = "collapsed" }
                                , { tag = "WriteToLocalStorage", key = "sidebar", value = "expanded" }
                                ]
                            )
            , test "starts collapsed when it was left collapsed, without animating" <|
                \() ->
                    start { defaultFlags | sidebar = Just "collapsed" }
                        |> ProgramTest.ensureViewHas [ attribute (Html.Attributes.attribute "data-state" "collapsed") ]
                        |> ProgramTest.ensureViewHas [ attribute (Html.Attributes.attribute "style" "--sidebar-progress: 1") ]
                        |> ProgramTest.expectModel (.sidebar >> Expect.equal { open = False, openMobile = False })
            , test "opening the mobile sheet keeps the wordmark expanded" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.update (Main.SidebarChanged { open = True, openMobile = True })
                        |> ProgramTest.update (Main.SidebarAnimated 100)
                        |> ProgramTest.expectModel (.sidebarProgress >> Expect.equal 0)
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
                        |> ProgramTest.ensureViewHas [ tag "h2", containing [ text "Unplug the cloud." ] ]
                        |> ProgramTest.expectViewHas
                            [ tag "section"
                            , containing
                                [ tag "a"
                                , attribute (Html.Attributes.href "/signup")
                                , containing [ text "Create an account" ]
                                ]
                            ]
            , test "lets a visitor who is signed out log in from the banner" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.ensureViewHasNot [ tag "section", containing [ tag "a", attribute (Html.Attributes.href "/studio") ] ]
                        |> ProgramTest.clickLink "Log in" "/login"
                        |> ProgramTest.expectBrowserUrl (Expect.equal "https://pixo.pics/login")
            , test "invites to sign up until it knows who is there" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.expectViewHas
                            [ tag "section"
                            , containing [ text "Unplug the cloud." ]
                            , containing [ tag "a", attribute (Html.Attributes.href "/signup") ]
                            ]
            , test "leads someone signed in to the studio" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.clickLink "Start creating" "/studio"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/studio")
                        |> ProgramTest.expectViewHas [ text "Nothing here yet" ]
            , test "keeps the banner for someone signed in, with a way down to the pictures" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.ensureViewHas [ tag "h2", containing [ text "Unplug the cloud." ] ]
                        |> ProgramTest.ensureViewHasNot [ tag "a", attribute (Html.Attributes.href "/signup") ]
                        |> ProgramTest.ensureViewHas [ id "gallery", containing [ text "See what people are making" ] ]
                        |> ProgramTest.ensureViewHasNot [ tag "section", containing [ tag "a", attribute (Html.Attributes.href "/login") ] ]
                        |> ProgramTest.clickLink "Get inspired" "#gallery"
                        |> ProgramTest.expectBrowserUrl (Expect.equal "https://pixo.pics/")
            , test "asks nothing of the browser" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [])
            ]
        , describe "Explore feed"
            [ test "shows the published pictures, each with its author" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/feed" (feedPage [ feedPicture feedIdA "ada" "a red fox" [ "fox" ], feedPicture feedIdB "ada" "a blue whale" [] ] Nothing)
                        |> ProgramTest.ensureViewHas [ tag "button", attribute (Html.Attributes.attribute "aria-label" "Open a red fox") ]
                        |> ProgramTest.ensureViewHas [ tag "button", attribute (Html.Attributes.attribute "aria-label" "Open a blue whale") ]
                        |> ProgramTest.expectViewHas [ tag "a", attribute (Html.Attributes.href "/ada"), containing [ text "ada" ] ]
            , test "loads more pictures after the last one" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/feed" (feedPage [ feedPicture feedIdA "ada" "a red fox" [ "fox" ] ] (Just feedIdA))
                        |> ProgramTest.clickButton "Load more"
                        |> ProgramTest.ensureViewHas [ text "Loading more…" ]
                        |> ProgramTest.ensureHttpRequest "GET" ("/api/feed/" ++ feedIdA) (\_ -> Expect.pass)
                        |> ProgramTest.simulateHttpOk "GET" ("/api/feed/" ++ feedIdA) (feedPage [ feedPicture feedIdB "ada" "a blue whale" [] ] Nothing)
                        |> ProgramTest.ensureViewHas [ tag "button", attribute (Html.Attributes.attribute "aria-label" "Open a red fox") ]
                        |> ProgramTest.ensureViewHas [ tag "button", attribute (Html.Attributes.attribute "aria-label" "Open a blue whale") ]
                        |> ProgramTest.ensureViewHasNot [ text "Load more" ]
                        |> ProgramTest.expectViewHasNot [ text "Loading more…" ]
            , test "says so and keeps the mosaic when the feed can't load" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/feed" (response 503 """{"error":"Unavailable right now."}""")
                        |> ProgramTest.ensureViewHas [ text "Can't load the feed" ]
                        |> ProgramTest.expectViewHas [ attribute (Html.Attributes.attribute "aria-hidden" "true"), containing [ tag "img" ] ]
            , test "shows the most used tags, and filters by one" <|
                \() ->
                    exploring
                        |> ProgramTest.ensureViewHas [ tag "button", containing [ text "All" ] ]
                        |> ProgramTest.clickButton "fox"
                        |> ProgramTest.ensureHttpRequest "GET" "/api/feed/tag/fox" (\_ -> Expect.pass)
                        |> ProgramTest.ensureViewHas [ tag "button", attribute (Html.Attributes.attribute "aria-pressed" "true"), containing [ text "fox" ] ]
                        |> ProgramTest.simulateHttpOk "GET" "/api/feed/tag/fox" (feedPage [ feedPicture feedIdB "ada" "a fox in a den" [ "fox" ] ] Nothing)
                        |> ProgramTest.ensureViewHas [ tag "button", attribute (Html.Attributes.attribute "aria-label" "Open a fox in a den") ]
                        |> ProgramTest.ensureViewHasNot [ tag "button", attribute (Html.Attributes.attribute "aria-label" "Open a red fox") ]
                        |> ProgramTest.clickButton "All"
                        |> ProgramTest.expectHttpRequest "GET" "/api/feed" (\_ -> Expect.pass)
            , test "ignores an answer for a tag that is no longer chosen" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/feed/tags" """[{"tag":"fox","pictures":2}]"""
                        |> ProgramTest.clickButton "fox"
                        |> ProgramTest.simulateHttpOk "GET" "/api/feed" (feedPage [ feedPicture feedIdA "ada" "a red fox" [ "fox" ] ] Nothing)
                        |> ProgramTest.expectViewHasNot [ tag "button", attribute (Html.Attributes.attribute "aria-label" "Open a red fox") ]
            , test "says when no picture has the tag" <|
                \() ->
                    exploring
                        |> ProgramTest.clickButton "fox"
                        |> ProgramTest.simulateHttpOk "GET" "/api/feed/tag/fox" (feedPage [] Nothing)
                        |> ProgramTest.expectViewHas [ text "No pictures with this tag yet" ]
            , test "leaves no pending load more behind when another tag is chosen" <|
                \() ->
                    exploring
                        |> ProgramTest.clickButton "Load more"
                        |> ProgramTest.clickButton "fox"
                        |> ProgramTest.simulateHttpOk "GET" "/api/feed/tag/fox" (feedPage [ feedPicture feedIdB "ada" "a fox in a den" [ "fox" ] ] (Just feedIdB))
                        |> ProgramTest.simulateHttpOk "GET" ("/api/feed/" ++ feedIdA) (feedPage [ feedPicture feedIdB "ada" "a blue whale" [] ] Nothing)
                        |> ProgramTest.expectViewHasNot [ text "Loading more…" ]
            , test "loads more under the chosen tag" <|
                \() ->
                    exploring
                        |> ProgramTest.clickButton "fox"
                        |> ProgramTest.simulateHttpOk "GET" "/api/feed/tag/fox" (feedPage [ feedPicture feedIdB "ada" "a fox in a den" [ "fox" ] ] (Just feedIdB))
                        |> ProgramTest.clickButton "Load more"
                        |> ProgramTest.expectHttpRequest "GET" ("/api/feed/tag/fox/" ++ feedIdB) (\_ -> Expect.pass)
            , test "filters by a tag picked in the picture, and shows it in the row" <|
                \() ->
                    exploring
                        |> ProgramTest.clickButton "Open a red fox"
                        |> inFeedPicture (ProgramTest.clickButton "snow")
                        |> ProgramTest.ensureHttpRequest "GET" "/api/feed/tag/snow" (\_ -> Expect.pass)
                        |> ProgramTest.ensureViewHasNot [ tag "button", attribute (Html.Attributes.attribute "aria-label" "Show details") ]
                        |> ProgramTest.expectViewHas [ tag "button", attribute (Html.Attributes.attribute "aria-pressed" "true"), containing [ text "snow" ] ]
            , test "closes the picture when its own tag is already chosen" <|
                \() ->
                    exploring
                        |> ProgramTest.clickButton "fox"
                        |> ProgramTest.simulateHttpOk "GET" "/api/feed/tag/fox" (feedPage [ feedPicture feedIdA "ada" "a red fox" [ "fox", "snow" ] ] Nothing)
                        |> ProgramTest.clickButton "Open a red fox"
                        |> inFeedPicture (ProgramTest.clickButton "fox")
                        |> ProgramTest.expectViewHasNot [ tag "button", attribute (Html.Attributes.attribute "aria-label" "Show details") ]
            ]
        , describe "Profile"
            [ test "asks for the profile of the name in the address" <|
                \() ->
                    startAt "/ada" defaultFlags
                        |> ProgramTest.expectHttpRequest "GET" "/api/profiles/ada" (\_ -> Expect.pass)
            , test "says so when there is no such profile" <|
                \() ->
                    startAt "/ada" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/profiles/ada" (response 404 """{"error":"No such profile."}""")
                        |> ProgramTest.expectViewHas [ text "Not found" ]
            , test "shows the name and the pictures" <|
                \() ->
                    startAt "/ada" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/profiles/ada" (profilePage "ada" [ feedPicture feedIdA "ada" "a red fox" [ "fox" ] ] Nothing)
                        |> ProgramTest.ensureViewHas [ tag "h1", containing [ text "ada" ] ]
                        |> ProgramTest.expectViewHas [ tag "button", attribute (Html.Attributes.attribute "aria-label" "Open a red fox") ]
            , test "offers Unpublish on one's own profile, which removes the picture" <|
                \() ->
                    startAt "/ada" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateHttpOk "GET" "/api/profiles/ada" (profilePage "ada" [ feedPicture feedIdA "ada" "a red fox" [ "fox" ] ] Nothing)
                        |> ProgramTest.clickButton "Open a red fox"
                        |> inProfilePicture (ProgramTest.clickButton "Unpublish")
                        |> ProgramTest.ensureHttpRequest "DELETE" ("/api/pictures/" ++ feedIdA) (\_ -> Expect.pass)
                        |> ProgramTest.simulateHttpResponse "DELETE" ("/api/pictures/" ++ feedIdA) (response 200 "{}")
                        |> ProgramTest.ensureViewHas [ text "Unpublished" ]
                        |> ProgramTest.expectViewHasNot [ tag "button", attribute (Html.Attributes.attribute "aria-label" "Open a red fox") ]
            , test "offers no Unpublish to a visitor who is signed out" <|
                \() ->
                    startAt "/ada" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.simulateHttpOk "GET" "/api/profiles/ada" (profilePage "ada" [ feedPicture feedIdA "ada" "a red fox" [ "fox" ] ] Nothing)
                        |> ProgramTest.clickButton "Open a red fox"
                        |> ProgramTest.ensureViewHas [ tag "button", attribute (Html.Attributes.attribute "aria-label" "Show details") ]
                        |> ProgramTest.expectViewHasNot [ tag "button", containing [ text "Unpublish" ] ]
            , test "offers no Unpublish on someone else's profile" <|
                \() ->
                    startAt "/grace" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateHttpOk "GET" "/api/profiles/grace" (profilePage "grace" [ feedPicture feedIdA "grace" "a red fox" [ "fox" ] ] Nothing)
                        |> ProgramTest.clickButton "Open a red fox"
                        |> ProgramTest.ensureViewHas [ tag "button", attribute (Html.Attributes.attribute "aria-label" "Show details") ]
                        |> ProgramTest.expectViewHasNot [ tag "button", containing [ text "Unpublish" ] ]
            , test "still opens the studio at /studio" <|
                \() ->
                    startAt "/studio" defaultFlags
                        |> ProgramTest.expectViewHas [ text "Nothing here yet" ]
            ]
        , describe "Studio"
            [ test "asks the browser whether it can run the model" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "CheckWebGpu" ])
            , test "keeps the form disabled until it knows the browser can run the model" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.ensureViewHas [ tag "button", attribute (Html.Attributes.disabled True), containing [ text "Generate" ] ]
                        |> ProgramTest.ensureViewHas [ tag "fieldset", attribute (Html.Attributes.disabled True), containing [ text "AI Style" ] ]
                        |> ProgramTest.ensureViewHas [ text "Checking GPU support" ]
                        |> ProgramTest.ensureViewHasNot [ class "invisible", containing [ text "Checking GPU support" ] ]
                        |> ProgramTest.ensureViewHas [ class "invisible", containing [ text "Nothing here yet" ] ]
                        |> ProgramTest.ensureViewHas [ class "invisible", containing [ text "Cannot generate images on this device" ] ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.ensureViewHas [ class "invisible", containing [ text "Checking GPU support" ] ]
                        |> ProgramTest.ensureViewHasNot [ class "invisible", containing [ text "Nothing here yet" ] ]
                        |> ProgramTest.ensureViewHas [ tag "fieldset", attribute (Html.Attributes.disabled False), containing [ text "AI Style" ] ]
                        |> ProgramTest.expectViewHas [ tag "button", attribute (Html.Attributes.disabled False), containing [ text "Generate" ] ]
            , test "makes pictures but writes no prompts without 16-bit shaders" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (adapterFound [ "subgroups" ])
                        |> ProgramTest.ensureViewHas [ tag "fieldset", attribute (Html.Attributes.disabled False), containing [ id "seed" ] ]
                        |> ProgramTest.ensureViewHas [ tag "button", attribute (Html.Attributes.disabled True), containing [ text "AI Style" ] ]
                        |> ProgramTest.ensureViewHas [ text "Needs 16-bit GPU shaders." ]
                        |> ProgramTest.expectViewHas [ tag "button", attribute (Html.Attributes.disabled False), containing [ text "Generate" ] ]
            , test "says nothing about 16-bit shaders when the GPU has them" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.ensureViewHas [ tag "button", attribute (Html.Attributes.disabled False), containing [ text "AI Style" ] ]
                        |> ProgramTest.expectViewHasNot [ text "Needs 16-bit GPU shaders." ]
            , test "says at once that a browser without the WebGPU API can't generate, without checking" <|
                \() ->
                    start { defaultFlags | webGpu = False }
                        |> ProgramTest.ensureViewHasNot [ class "invisible", containing [ text "Cannot generate images on this device" ] ]
                        |> ProgramTest.ensureViewHas [ tag "fieldset", attribute (Html.Attributes.disabled True), containing [ text "AI Style" ] ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [])
            , test "says so when the browser has no WebGPU" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked False)
                        |> ProgramTest.ensureViewHasNot [ class "invisible", containing [ text "Cannot generate images on this device" ] ]
                        |> ProgramTest.ensureViewHas [ class "invisible", containing [ text "Nothing here yet" ] ]
                        |> ProgramTest.ensureViewHas [ tag "fieldset", attribute (Html.Attributes.disabled True), containing [ text "AI Style" ] ]
                        |> ProgramTest.ensureViewHas [ tag "fieldset", attribute (Html.Attributes.disabled True), containing [ id "seed" ] ]
                        |> ProgramTest.expectViewHas [ tag "button", attribute (Html.Attributes.disabled True), containing [ text "Generate" ] ]
            , test "says it can't generate when the worker failed on another page" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (pipelineFailed "Error" "Worker unable to start")
                        |> ProgramTest.clickLink "Studio" "/studio"
                        |> ProgramTest.ensureViewHasNot [ class "invisible", containing [ text "Cannot generate images on this device" ] ]
                        |> ProgramTest.ensureViewHas [ text "Worker unable to start" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [])
            , test "downloads the model, then makes a picture from the prompt, the size, the steps and the seed" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" "  a bonsai on a mountain "
                        |> ProgramTest.fillIn "seed" "Seed" "7"
                        |> ProgramTest.clickButton "Generate"
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
                        |> ProgramTest.ensureViewHas [ text "Downloading the transformer" ]
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
                                      , width = 1024
                                      , height = 1024
                                      , numInferenceSteps = 4
                                      , seed = 7
                                      , guidanceScale = 1
                                      , frameBudget = 8
                                      }
                                    ]
                            )
                        |> ProgramTest.ensureViewHas [ text "Generating · Step 1 of 4" ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (generationStep 0)
                        |> ProgramTest.ensureViewHas [ text "Generating · Step 2 of 4" ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (generationStep 3)
                        |> ProgramTest.ensureViewHas [ text "Finishing…" ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.ensureViewHas [ tag "img", attribute (Html.Attributes.src "blob:https://pixo.pics/1") ]
                        |> ProgramTest.expectViewHas [ text "1024 × 1024 · 4 steps · seed 7" ]
            , test "remembers the model it downloaded" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
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
                        |> ProgramTest.clickButton "Binary, 1-bit"
                        |> ProgramTest.ensureViewHas
                            [ tag "button", id "weights", containing [ text "Binary, 1-bit" ] ]
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
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
                        |> ProgramTest.ensureViewHas [ id "size", attribute (Html.Attributes.attribute "aria-label" "Size: Story, 9:16"), containing [ text "9:16" ] ]
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            generation
                            (List.filterMap (Maybe.map (\made -> ( made.width, made.height )))
                                >> Expect.equal [ ( 576, 1024 ) ]
                            )
            , test "starts square, with no sliders for the size" <|
                \() ->
                    ready
                        |> ProgramTest.ensureViewHas [ id "size", attribute (Html.Attributes.attribute "aria-label" "Size: Square, 1:1"), containing [ text "1:1" ] ]
                        |> ProgramTest.ensureViewHas
                            [ attribute (Html.Attributes.attribute "role" "menuitemradio")
                            , attribute (Html.Attributes.attribute "aria-checked" "true")
                            , containing [ text "Square" ]
                            ]
                        |> ProgramTest.ensureView (Query.findAll [ attribute (Html.Attributes.attribute "aria-checked" "true") ] >> Query.count (Expect.equal 1))
                        |> ProgramTest.ensureViewHasNot [ attribute (Html.Attributes.attribute "role" "slider") ]
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            generation
                            (List.filterMap (Maybe.map (\made -> ( made.width, made.height )))
                                >> Expect.equal [ ( 1024, 1024 ) ]
                            )
            , test "shows random until a seed is typed, and goes back to it when emptied" <|
                \() ->
                    ready
                        |> ProgramTest.ensureViewHas [ id "seed", attribute (Html.Attributes.value ""), attribute (Html.Attributes.placeholder "-1") ]
                        |> ProgramTest.fillIn "seed" "Seed" "31"
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            generation
                            (List.filterMap (Maybe.map .seed) >> Expect.equal [ 31 ])
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.ensureViewHas [ id "seed", attribute (Html.Attributes.value "31") ]
                        |> ProgramTest.fillIn "seed" "Seed" ""
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            generation
                            (List.filterMap (Maybe.map .seed) >> List.drop 1 >> List.all ((/=) 31) >> Expect.equal True)
            , test "says what the browser can't read as a seed, and makes no picture" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.fillIn "seed" "Seed" "123"
                        |> ProgramTest.simulateDomEvent (Query.find [ id "seed" ]) ( "input", Encode.object [ ( "target", Encode.object [ ( "value", Encode.string "" ), ( "validity", Encode.object [ ( "badInput", Encode.bool True ) ] ) ] ) ] )
                        |> ProgramTest.ensureViewHas [ attribute (Html.Attributes.attribute "role" "alert"), containing [ text "Seed must be a whole number from 0 to 4294967295." ] ]
                        |> ProgramTest.ensureViewHas [ id "seed", attribute (Html.Attributes.attribute "aria-invalid" "true"), attribute (Html.Attributes.value "") ]
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            generation
                            (List.filterMap identity >> Expect.equal [])
            , test "makes a picture with Cmd or Ctrl and Enter in the prompt" <|
                \() ->
                    let
                        pressing meta ctrl =
                            ready
                                |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                                |> ProgramTest.simulateDomEvent (Query.find [ id "prompt" ])
                                    ( "keydown"
                                    , Encode.object
                                        [ ( "key", Encode.string "Enter" )
                                        , ( "metaKey", Encode.bool meta )
                                        , ( "ctrlKey", Encode.bool ctrl )
                                        ]
                                    )
                                |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                                    generation
                                    (List.filterMap (Maybe.map .prompt) >> Expect.equal [ "a bonsai" ])
                    in
                    Expect.all [ \() -> pressing True False, \() -> pressing False True ] ()
            , test "takes the steps typed" <|
                \() ->
                    ready
                        |> ProgramTest.ensureViewHas [ id "steps", attribute (Html.Attributes.attribute "inputmode" "numeric"), attribute (Html.Attributes.value "4") ]
                        |> ProgramTest.ensureViewHas [ id "steps", attribute (Html.Attributes.type_ "number"), attribute (Html.Attributes.min "1"), attribute (Html.Attributes.max "50"), attribute (Html.Attributes.step "1") ]
                        |> ProgramTest.ensureViewHas [ id "seed", attribute (Html.Attributes.type_ "number"), attribute (Html.Attributes.min "0"), attribute (Html.Attributes.max "4294967295"), attribute (Html.Attributes.step "1") ]
                        |> ProgramTest.ensureViewHas [ tag "form", attribute (Html.Attributes.novalidate True) ]
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.fillIn "steps" "Steps" "8"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.ensureViewHas [ text "Generating · Step 1 of 8" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            generation
                            (List.filterMap (Maybe.map .numInferenceSteps) >> Expect.equal [ 8 ])
            , test "keeps the controls while it generates, and counts the steps on the picture" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.ensureViewHas
                            [ attribute (Html.Attributes.attribute "data-slot" "card-footer")
                            , containing [ id "size" ]
                            , containing [ id "seed" ]
                            , containing [ id "steps" ]
                            , containing [ tag "button", attribute (Html.Attributes.attribute "aria-label" "Cancel") ]
                            ]
                        |> ProgramTest.ensureViewHasNot [ attribute (Html.Attributes.attribute "data-slot" "card-footer"), containing [ text "Generating · Step 1 of 4" ] ]
                        |> ProgramTest.ensureViewHasNot [ attribute (Html.Attributes.attribute "data-slot" "card-footer"), containing [ attribute (Html.Attributes.attribute "role" "progressbar") ] ]
                        |> ProgramTest.ensureViewHas
                            [ attribute (Html.Attributes.style "aspect-ratio" "1024 / 1024")
                            , containing [ attribute (Html.Attributes.attribute "role" "progressbar"), containing [ text "Generating · Step 1 of 4" ], containing [ text "0%" ] ]
                            ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (generationStep 0)
                        |> ProgramTest.ensureViewHas
                            [ attribute (Html.Attributes.style "aspect-ratio" "1024 / 1024")
                            , containing [ attribute (Html.Attributes.attribute "role" "progressbar"), containing [ text "Generating · Step 2 of 4" ], containing [ text "25%" ] ]
                            ]
                        |> ProgramTest.clickButton "Cancel"
                        |> ProgramTest.expectViewHas [ attribute (Html.Attributes.attribute "data-slot" "card-footer"), containing [ tag "button", attribute (Html.Attributes.disabled True), attribute (Html.Attributes.attribute "aria-label" "Cancelling…") ] ]
            , test "locks every control but Cancel while it makes a picture" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.ensureView (lockedBut (attribute (Html.Attributes.attribute "aria-label" "Cancel")))
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.expectView (Query.findAll [ id "prompt", attribute (Html.Attributes.disabled False) ] >> Query.count (Expect.equal 1))
            , test "says it is cancelling a download until it stops" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (modelProgress "transformer" 599000000 1425271472 False)
                        |> ProgramTest.clickButton "Cancel"
                        |> ProgramTest.ensureViewHas
                            [ attribute (Html.Attributes.attribute "data-slot" "card-footer")
                            , containing [ text "Downloading the transformer" ]
                            , containing [ tag "button", attribute (Html.Attributes.disabled True), attribute (Html.Attributes.attribute "aria-label" "Cancelling…") ]
                            ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (pipelineFailed "AbortError" "")
                        |> ProgramTest.ensureViewHasNot [ attribute (Html.Attributes.attribute "aria-label" "Cancelling…") ]
                        |> ProgramTest.expectViewHas [ tag "button", attribute (Html.Attributes.disabled False), containing [ text "Generate" ] ]
            , test "says how many steps it takes" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.fillIn "steps" "Steps" "80"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.ensureViewHas [ text "Steps must be a whole number from 1 to 50." ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            generation
                            (List.filterMap identity >> Expect.equal [])
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
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.ensureViewHas [ attribute (Html.Attributes.attribute "role" "alert"), containing [ text "Write a prompt to generate a picture." ] ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "CheckWebGpu" ])
            , test "says what is wrong with the steps as they are typed" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "steps" "Steps" "80"
                        |> ProgramTest.ensureViewHas [ attribute (Html.Attributes.attribute "role" "alert"), containing [ text "Steps must be a whole number from 1 to 50." ] ]
                        |> ProgramTest.ensureViewHas [ id "steps", attribute (Html.Attributes.attribute "aria-invalid" "true") ]
                        |> ProgramTest.ensureViewHasNot [ attribute (Html.Attributes.attribute "role" "alert"), containing [ text "Write a prompt to generate a picture." ] ]
                        |> ProgramTest.fillIn "steps" "Steps" "8"
                        |> ProgramTest.expectViewHasNot [ attribute (Html.Attributes.attribute "role" "alert") ]
            , test "hides the prompt problem once a prompt is typed" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.ensureViewHas [ id "prompt", attribute (Html.Attributes.attribute "aria-invalid" "true") ]
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.ensureViewHas [ id "prompt", attribute (Html.Attributes.attribute "aria-invalid" "false") ]
                        |> ProgramTest.ensureViewHasNot [ attribute (Html.Attributes.attribute "role" "alert") ]
                        |> ProgramTest.expectViewHas [ attribute (Html.Attributes.attribute "inert" ""), containing [ text "Write a prompt to generate a picture." ] ]
            , test "says why the model did not load" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (pipelineFailed "Error" "HTTP 404 for model_index.json")
                        |> ProgramTest.ensureViewHas [ text "HTTP 404 for model_index.json" ]
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.expectViewHas [ attribute (Html.Attributes.attribute "inert" ""), containing [ text "HTTP 404 for model_index.json" ] ]
            , test "stops a picture when asked, and is ready for the next" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.clickButton "Cancel"
                        |> ProgramTest.ensureViewHas [ text "Cancelling…" ]
                        |> ProgramTest.ensureViewHasNot [ text "Stopping…" ]
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
                        |> ProgramTest.clickLink "Settings" "/settings/general"
                        |> ProgramTest.clickLink "Models" "/settings/models"
                        |> ProgramTest.ensureViewHas [ tag "button", containing [ text "Release" ] ]
                        |> ProgramTest.clickLink "Studio" "/studio"
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [])
            , test "saves a picture to the library with everything that made it" <|
                \() ->
                    ready
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.fillIn "seed" "Seed" "7"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.clickButton "Save"
                        |> ProgramTest.ensureViewHas [ text "Saving…" ]
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.maybe pictureSave)
                            (List.filterMap identity
                                >> Expect.equal
                                    [ { tag = "IdbAdd"
                                      , store = "pictures"
                                      , blobs = [ ( "blob", "blob:https://pixo.pics/1" ) ]
                                      , label = "blob:https://pixo.pics/1"
                                      , prompt = "a bonsai"
                                      , width = 1024
                                      , height = 1024
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
                            (Expect.equal [ "IdbGetAll" ])
            , test "tags a saved picture with the prompt model" <|
                \() ->
                    readyToTag
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.clickButton "Save"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "GenerateImage", "IdbAdd" ])
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (pictureSaved "blob:https://pixo.pics/1" (stored 1 "blob:https://pixo.pics/2" "a bonsai" 512 512))
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            outgoing
                            (Expect.equal [ ( "LoadTagger", Just "Qwen3-1.7B-q4f16_1-MLC" ) ])
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "TaggerLoaded")
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string |> Decode.andThen (\name -> Decode.map (Tuple.pair name) (Decode.maybe (Decode.at [ "data", "messages" ] (Decode.map lastTurn (Decode.list turn))))))
                            (Expect.equal [ ( "TagPicture", Just (Just ( "user", "a bonsai" )) ) ])
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (Encode.object [ ( "tag", Encode.string "TagsWritten" ), ( "text", Encode.string "bonsai, tree, studio" ) ])
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            patchOrRelease
                            (List.sort
                                >> Expect.equal
                                    [ "IdbPatch:1:tags:[\"bonsai\",\"tree\",\"studio\"]"
                                    , "ReleaseTagger"
                                    ]
                            )
            , test "says that it is tagging" <|
                \() ->
                    readyToTag
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.clickButton "Save"
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (pictureSaved "blob:https://pixo.pics/1" (stored 1 "blob:https://pixo.pics/2" "a bonsai" 512 512))
                        |> ProgramTest.ensureViewHas
                            [ attribute (Html.Attributes.attribute "role" "status"), containing [ text "Tagging a picture" ] ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "TaggerLoaded")
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (Encode.object [ ( "tag", Encode.string "TagsWritten" ), ( "text", Encode.string "bonsai, tree, studio" ) ])
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "TaggerReleased")
                        |> ProgramTest.expectViewHasNot [ attribute (Html.Attributes.attribute "role" "status"), containing [ text "Tagging a picture" ] ]
            , test "says that it is generating" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (generationStep 0)
                        |> ProgramTest.expectViewHas [ attribute (Html.Attributes.attribute "role" "status"), containing [ text "Generating a picture" ] ]
            , test "names the logo pixo while it works" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (generationStep 0)
                        |> ProgramTest.clickLink "pixo" "/"
                        |> ProgramTest.expectBrowserUrl (String.endsWith "/" >> Expect.equal True)
            , test "saves without tags when the prompt model is not downloaded" <|
                \() ->
                    ready
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.clickButton "Save"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "GenerateImage", "IdbAdd" ])
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (pictureSaved "blob:https://pixo.pics/1" (stored 1 "blob:https://pixo.pics/2" "a bonsai" 512 512))
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [])
            , test "tags the library's older pictures once the prompt model can run" <|
                \() ->
                    startAt "/library" { defaultFlags | stylist = Just "Qwen3-1.7B-q4f16_1-MLC" }
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateIncomingPort "interopToElm" olderPictures
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            outgoing
                            (List.filter (Tuple.first >> (==) "LoadTagger") >> Expect.equal [])
                        |> ProgramTest.clickLink "Settings" "/settings/general"
                        |> ProgramTest.clickLink "Models" "/settings/models"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            outgoing
                            (List.filter (Tuple.first >> (==) "LoadTagger") >> Expect.equal [ ( "LoadTagger", Just "Qwen3-1.7B-q4f16_1-MLC" ) ])
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "TaggerLoaded")
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string |> Decode.andThen (\name -> Decode.map (Tuple.pair name) (Decode.maybe (Decode.at [ "data", "messages" ] (Decode.map lastTurn (Decode.list turn))))))
                            (Expect.equal [ ( "TagPicture", Just (Just ( "user", "a city skyline at sunset" )) ) ])
            , test "leaves older pictures alone without the prompt model" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateIncomingPort "interopToElm" olderPictures
                        |> ProgramTest.clickLink "Settings" "/settings/general"
                        |> ProgramTest.clickLink "Models" "/settings/models"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (List.member "LoadTagger" >> Expect.equal False)
            , test "keeps the prompt writer while it tags" <|
                \() ->
                    readyToTag
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.clickButton "Save"
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (pictureSaved "blob:https://pixo.pics/1" (stored 1 "blob:https://pixo.pics/2" "a bonsai" 512 512))
                        |> ProgramTest.clickLink "Settings" "/settings/general"
                        |> ProgramTest.clickLink "Models" "/settings/models"
                        |> ProgramTest.clickButton "Remove the prompt writer"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (List.member "RemoveStylist" >> Expect.equal False)
                        |> ProgramTest.expectViewHasNot [ text "Prompt model removed" ]
            , test "opens a new picture bigger, with everything that made it" <|
                \() ->
                    ready
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.ensureViewHas
                            [ tag "figcaption", containing [ text "Bonsai ternary, 2-bit" ] ]
                        |> ProgramTest.ensureViewHasNot [ tag "elmcn-dialog", attribute (Html.Attributes.attribute "open" "") ]
                        |> ProgramTest.clickButton "Open a bonsai"
                        |> ProgramTest.ensureViewHas [ tag "elmcn-dialog", attribute (Html.Attributes.attribute "open" "") ]
                        |> ProgramTest.ensureViewHas
                            [ id "studio-picture", containing [ text "1024 × 1024 · 4 steps · seed " ], containing [ text "Bonsai ternary, 2-bit" ] ]
                        |> ProgramTest.within
                            (Query.find [ id "studio-picture" ])
                            (ProgramTest.clickButton "Save")
                        |> ProgramTest.ensureViewHas [ text "Saving…" ]
                        |> ProgramTest.clickButton "Close"
                        |> ProgramTest.expectViewHasNot [ tag "elmcn-dialog", attribute (Html.Attributes.attribute "open" "") ]
            , test "shows the picture made before with the right arrow key" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.fillIn "prompt" "Prompt" "a maple"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/2")
                        |> ProgramTest.clickButton "Open a maple"
                        |> ProgramTest.simulateDomEvent
                            (Query.find [ attribute (Html.Attributes.attribute "data-slot" "lightbox") ])
                            ( "keydown", Encode.object [ ( "key", Encode.string "ArrowRight" ) ] )
                        |> ProgramTest.expectViewHas
                            [ id "studio-picture", containing [ tag "img", attribute (Html.Attributes.alt "a bonsai") ] ]
            , test "keeps a picture's details over it once it is tapped, until the next tap" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.clickButton "Open a bonsai"
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
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.clickButton "Save"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (libraryFailed "The quota has been exceeded.")
                        |> ProgramTest.ensureViewHasNot [ text "Saving…" ]
                        |> ProgramTest.clickButton "Save"
                        |> ProgramTest.expectViewHas [ text "Saving…" ]
            , test "offers to log in to save when signed out" <|
                \() ->
                    ready
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.ensureViewHas [ tag "a", attribute (Html.Attributes.href "/login"), containing [ text "Log in to save" ] ]
                        |> ProgramTest.expectViewHasNot [ tag "button", containing [ text "Save" ] ]
            ]
        , describe "AI Style"
            [ test "loads no prompt model on a browser that can't run it" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked False)
                        |> ProgramTest.update (Main.GotStylist (Stylist.StyleChosen Stylist.Photo { width = 1024, height = 1024 } "a fox in the snow"))
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "CheckWebGpu" ])
            , test "loads no prompt model on a GPU without 16-bit shaders" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (adapterFound [])
                        |> ProgramTest.update (Main.GotStylist (Stylist.StyleChosen Stylist.Photo { width = 1024, height = 1024 } "a fox in the snow"))
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "CheckWebGpu" ])
            , test "says what the first rewrite downloads" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.expectViewHas [ text "Downloads 984 MB once" ]
            , test "says nothing of a download it already has" <|
                \() ->
                    start { defaultFlags | stylist = Just "Qwen3-1.7B-q4f16_1-MLC" }
                        |> ProgramTest.expectViewHasNot [ text "Downloads 984 MB once" ]
            , test "asks for an idea first" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.clickButton "Photo"
                        |> ProgramTest.ensureViewHas [ attribute (Html.Attributes.attribute "role" "alert"), containing [ text "Write an idea for AI Style to rewrite." ] ]
                        |> ProgramTest.fillIn "prompt" "Prompt" "a fox"
                        |> ProgramTest.ensureViewHasNot [ attribute (Html.Attributes.attribute "role" "alert") ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "CheckWebGpu" ])
            , test "loads the prompt model, then rewrites the idea as it streams in" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" " a fox in the snow "
                        |> ProgramTest.clickButton "Photo"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string |> Decode.andThen (\name -> Decode.map (Tuple.pair name) (Decode.maybe (Decode.at [ "data", "model" ] Decode.string))))
                            (Expect.equal [ ( "CheckWebGpu", Nothing ), ( "LoadStylist", Just "Qwen3-1.7B-q4f16_1-MLC" ) ])
                        |> ProgramTest.simulateIncomingPort "interopToElm" (stylistProgress 0.25)
                        |> ProgramTest.ensureViewHas [ text "Downloading the prompt model" ]
                        |> ProgramTest.ensureViewHas [ text "25%" ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "StylistLoaded")
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            writing
                            (List.filterMap identity
                                >> Expect.equal
                                    [ { model = "Qwen3-1.7B-q4f16_1-MLC"
                                      , roles = [ "system", "user", "assistant", "user", "assistant", "user" ]
                                      , idea = "a fox in the snow"
                                      , thinking = False
                                      }
                                    ]
                            )
                        |> ProgramTest.ensureViewHasNot [ text "Writing a Photo prompt…" ]
                        |> ProgramTest.ensureViewHas [ tag "button", containing [ text "Stop generating" ] ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (promptStreamed "<think>\n\n</think>\n\nSubject: a red fox curled up in fresh snow\nSty")
                        |> ProgramTest.ensureViewHas [ id "prompt", attribute (Html.Attributes.value "Subject: a red fox curled up in fresh snow"), attribute (Html.Attributes.disabled True) ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (promptWritten ("<think>\n\n</think>\n\n" ++ foxPrompt))
                        |> ProgramTest.ensureViewHas [ id "prompt", attribute (Html.Attributes.value foxPrompt) ]
                        |> ProgramTest.expectViewHas [ tag "button", attribute (Html.Attributes.attribute "aria-label" "Undo") ]
            , test "writes for the size chosen" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.clickButton "Widescreen"
                        |> ProgramTest.fillIn "prompt" "Prompt" "a fox in the snow"
                        |> ProgramTest.clickButton "Photo"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "StylistLoaded")
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            instructing
                            (List.filterMap identity
                                >> List.map (String.contains "The picture is a wide landscape frame.")
                                >> Expect.equal [ True ]
                            )
            , test "can't generate while it writes the prompt" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" "a fox in the snow"
                        |> ProgramTest.clickButton "Photo"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "StylistLoaded")
                        |> ProgramTest.simulateIncomingPort "interopToElm" (promptStreamed "Subject: a red fox")
                        |> ProgramTest.ensureViewHas [ tag "button", attribute (Html.Attributes.disabled True), containing [ text "Generate" ] ]
                        |> ProgramTest.update (Main.GotPipeline Pipeline.GenerateClicked)
                        |> ProgramTest.ensureViewHasNot [ attribute (Html.Attributes.attribute "role" "progressbar") ]
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (List.filter (\name -> name == "LoadModel" || name == "GenerateImage") >> Expect.equal [])
                        |> ProgramTest.simulateIncomingPort "interopToElm" (promptWritten foxPrompt)
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "ModelLoaded")
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            generation
                            (List.filterMap (Maybe.map .prompt) >> Expect.equal [ foxPrompt ])
            , test "shows the prompt model loading in place of the settings, and cancels the rewrite" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" "a fox in the snow"
                        |> ProgramTest.clickButton "Photo"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (stylistProgress 0.42)
                        |> ProgramTest.ensureViewHas
                            [ attribute (Html.Attributes.attribute "data-slot" "card-footer")
                            , containing [ text "Downloading the prompt model" ]
                            , containing [ text "42%" ]
                            , containing [ tag "button", attribute (Html.Attributes.attribute "aria-label" "Cancel") ]
                            ]
                        |> ProgramTest.ensureViewHasNot [ id "steps" ]
                        |> ProgramTest.ensureViewHas [ tag "button", id "ai-style", attribute (Html.Attributes.disabled True) ]
                        |> ProgramTest.clickButton "Cancel"
                        |> ProgramTest.ensureViewHasNot [ text "Downloading the prompt model" ]
                        |> ProgramTest.ensureViewHas [ id "steps" ]
                        |> ProgramTest.expectViewHas [ id "prompt", attribute (Html.Attributes.value "a fox in the snow"), attribute (Html.Attributes.disabled False) ]
            , test "locks every control but Stop generating while it writes" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" "a fox in the snow"
                        |> ProgramTest.clickButton "Photo"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "StylistLoaded")
                        |> ProgramTest.simulateIncomingPort "interopToElm" (promptStreamed "Subject: a red fox")
                        |> ProgramTest.ensureView (lockedBut (containing [ text "Stop generating" ]))
                        |> ProgramTest.simulateIncomingPort "interopToElm" (promptWritten foxPrompt)
                        |> ProgramTest.expectView (Query.findAll [ id "prompt", attribute (Html.Attributes.disabled False) ] >> Query.count (Expect.equal 1))
            , test "loads the prompt model it already has, rather than downloading it" <|
                \() ->
                    start { defaultFlags | stylist = Just "Qwen3-1.7B-q4f16_1-MLC" }
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" "a fox in the snow"
                        |> ProgramTest.clickButton "Photo"
                        |> ProgramTest.ensureViewHasNot [ text "Downloading the prompt model" ]
                        |> ProgramTest.expectViewHas [ text "Loading the prompt model" ]
            , test "remembers the prompt model it downloaded" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" "a fox in the snow"
                        |> ProgramTest.clickButton "Photo"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "StylistLoaded")
                        |> ProgramTest.ensureViewHasNot [ text "Downloads 984 MB once" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.maybe localStorageWrite)
                            (List.filterMap identity
                                >> Expect.equal
                                    [ { tag = "WriteToLocalStorage", key = "stylist", value = "Qwen3-1.7B-q4f16_1-MLC" } ]
                            )
            , test "gives the idea back on Undo" <|
                \() ->
                    styled
                        |> ProgramTest.clickButton "Undo"
                        |> ProgramTest.ensureViewHasNot [ attribute (Html.Attributes.attribute "aria-label" "Undo") ]
                        |> ProgramTest.expectViewHas [ id "prompt", attribute (Html.Attributes.value "a fox in the snow") ]
            , test "rewrites the first idea, not the last prompt, in another style" <|
                \() ->
                    styled
                        |> ProgramTest.clickButton "Anime"
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            writing
                            (List.filterMap (Maybe.map .idea) >> Expect.equal [ "a fox in the snow" ])
            , test "rewrites an edited prompt as it is" <|
                \() ->
                    styled
                        |> ProgramTest.fillIn "prompt" "Prompt" "a fox in the rain"
                        |> ProgramTest.ensureViewHasNot [ attribute (Html.Attributes.attribute "aria-label" "Undo") ]
                        |> ProgramTest.clickButton "Anime"
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            writing
                            (List.filterMap (Maybe.map .idea) >> Expect.equal [ "a fox in the rain" ])
            , test "stops writing and gives the idea back" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" "a fox in the snow"
                        |> ProgramTest.clickButton "Photo"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "StylistLoaded")
                        |> ProgramTest.simulateIncomingPort "interopToElm" (promptStreamed "Subject: a red fox")
                        |> ProgramTest.clickButton "Stop generating"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (List.reverse >> List.head >> Expect.equal (Just "StopWriting"))
                        |> ProgramTest.ensureViewHas [ tag "button", attribute (Html.Attributes.disabled True), containing [ text "Stopping…" ] ]
                        |> ProgramTest.ensureViewHasNot [ tag "button", containing [ text "Stop generating" ] ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (promptWritten "Subject: a red fox")
                        |> ProgramTest.ensureViewHasNot [ attribute (Html.Attributes.attribute "aria-label" "Undo") ]
                        |> ProgramTest.expectViewHas [ id "prompt", attribute (Html.Attributes.value "a fox in the snow") ]
            , test "keeps the idea when the answer is not a prompt" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" "a fox in the snow"
                        |> ProgramTest.clickButton "Photo"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "StylistLoaded")
                        |> ProgramTest.simulateIncomingPort "interopToElm" (promptWritten "\"Cozy\"")
                        |> ProgramTest.ensureViewHas [ text "Incomplete prompt" ]
                        |> ProgramTest.expectViewHas [ id "prompt", attribute (Html.Attributes.value "a fox in the snow") ]
            , test "shows why the prompt model failed, and keeps the idea" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" "a fox in the snow"
                        |> ProgramTest.clickButton "Photo"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (stylistFailed "Error" "Out of memory.")
                        |> ProgramTest.ensureViewHas [ text "Out of memory." ]
                        |> ProgramTest.expectViewHas [ id "prompt", attribute (Html.Attributes.value "a fox in the snow") ]
            ]
        , describe "General"
            [ test "checks the server as it opens, with its release, region and latency" <|
                \() ->
                    startAt "/settings/general" defaultFlags
                        |> ProgramTest.ensureViewHas [ text "Checking health status" ]
                        |> ProgramTest.ensureViewHasNot [ text "Region" ]
                        |> ProgramTest.simulateHttpOk "GET"
                            "/api/health"
                            """{"status":"pass","releaseId":"7990ba2c0ffee","region":"fra1","links":{"release":"https://github.com/ricardocasares/pixo.pics/commit/7990ba2c0ffee"},"checks":{"database":{"status":"pass","latencyMs":25}}}"""
                        |> ProgramTest.ensureViewHasNot [ text "Checking health status" ]
                        |> ProgramTest.ensureViewHas [ text "Healthy" ]
                        |> ProgramTest.ensureViewHas [ text "fra1" ]
                        |> ProgramTest.ensureViewHas [ text "25 ms" ]
                        |> ProgramTest.expectViewHas
                            [ tag "a"
                            , attribute (Html.Attributes.href "https://github.com/ricardocasares/pixo.pics/commit/7990ba2c0ffee")
                            , containing [ text "7990ba2" ]
                            ]
            , test "says that prompts and pictures stay on this device unless shared" <|
                \() ->
                    startAt "/settings/general" defaultFlags
                        |> ProgramTest.expectViewHas [ text "Your prompts and pictures stay on this device unless you decide to share them on the public feed." ]
            , test "checks again each time it is opened" <|
                \() ->
                    startAt "/settings/general" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/health" health
                        |> ProgramTest.clickLink "Models" "/settings/models"
                        |> ProgramTest.clickLink "General" "/settings/general"
                        |> ProgramTest.expectHttpRequestWasMade "GET" "/api/health"
            , test "says why the database is slow" <|
                \() ->
                    startAt "/settings/general" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET"
                            "/api/health"
                            """{"status":"warn","region":"fra1","checks":{"database":{"status":"warn","latencyMs":1500,"output":"Database slow, over 1000 ms"}}}"""
                        |> ProgramTest.ensureViewHas [ text "1500 ms" ]
                        |> ProgramTest.expectViewHas [ text "Database slow, over 1000 ms" ]
            , test "reads a failed check from its 503" <|
                \() ->
                    startAt "/settings/general" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET"
                            "/api/health"
                            (response 503 """{"status":"fail","checks":{"database":{"status":"fail","latencyMs":5000,"output":"Database timed out after 5000 ms"}}}""")
                        |> ProgramTest.ensureView
                            (Query.find [ attribute (Html.Attributes.attribute "data-slot" "item") ]
                                >> Query.has [ containing [ text "Database timed out after 5000 ms" ] ]
                            )
                        |> ProgramTest.expectViewHasNot [ text "Region" ]
            , test "keeps checking and asks again on request" <|
                \() ->
                    startAt "/settings/general" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/health" Test.Http.networkError
                        |> ProgramTest.ensureViewHas [ text "Checking health status" ]
                        |> inToast "Can't check health" (ProgramTest.clickButton "Try again")
                        |> ProgramTest.simulateHttpOk "GET" "/api/health" health
                        |> ProgramTest.expectViewHas [ text "Healthy" ]
            , test "says when the server can't be reached" <|
                \() ->
                    startAt "/settings/general" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/health" Test.Http.networkError
                        |> ProgramTest.expectViewHas [ text "Network offline" ]
            , test "is listed in the settings" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.clickLink "General" "/settings/general"
                        |> ProgramTest.expectHttpRequestWasMade "GET" "/api/health"
            ]
        , describe "Models"
            [ test "says the prompt writer needs 16-bit shaders, and still offers the image models" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (adapterFound [])
                        |> ProgramTest.ensureViewHas [ text "Needs 16-bit GPU shaders." ]
                        |> ProgramTest.ensureViewHasNot [ class "invisible", containing [ text "Needs 16-bit GPU shaders." ] ]
                        |> ProgramTest.ensureViewHas [ class "invisible", containing [ text "Downloads the first time you use AI Style in the studio." ] ]
                        |> ProgramTest.expectViewHasNot [ class "invisible", containing [ text "Download once, use in the studio." ] ]
            , test "removes the prompt model it downloaded" <|
                \() ->
                    startAt "/settings/models" { defaultFlags | stylist = Just "Qwen3-1.7B-q4f16_1-MLC" }
                        |> ProgramTest.clickButton "Remove the prompt writer"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.map2 Tuple.pair (Decode.field "tag" Decode.string) (Decode.maybe (Decode.at [ "data", "model" ] Decode.string)))
                            (List.reverse >> List.head >> Expect.equal (Just ( "RemoveStylist", Just "Qwen3-1.7B-q4f16_1-MLC" )))
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "StylistRemoved")
                        |> ProgramTest.ensureViewHas [ text "Prompt model removed" ]
                        |> ProgramTest.ensureViewHas [ text "Qwen3 1.7B, 984 MB download" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.maybe localStorageWrite)
                            (List.filterMap identity
                                >> Expect.equal [ { tag = "WriteToLocalStorage", key = "stylist", value = "" } ]
                            )
            , test "lists every model with what a download takes" <|
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
                        |> ProgramTest.ensureViewHas [ text "Downloading the transformer" ]
                        |> ProgramTest.ensureViewHas [ text "50%" ]
                        |> ProgramTest.ensureViewHas
                            [ attribute (Html.Attributes.attribute "data-slot" "card-footer")
                            , containing [ text "50%" ]
                            , containing [ tag "button", containing [ text "Cancel" ] ]
                            ]
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
                        |> ProgramTest.ensureViewHas [ tag "button", containing [ text "Release" ] ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.maybe localStorageWrite)
                            (List.filterMap (Maybe.map .value)
                                >> Expect.equal [ """["prism-ml/bonsai-image-ternary-4B-mlx-2bit"]""" ]
                            )
            , test "downloads again from the failure's toast" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.clickButton "Download Bonsai ternary, 2-bit"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (pipelineFailed "Error" "HTTP 404 for model_index.json")
                        |> inToast "Couldn't download" (ProgramTest.clickButton "Try again")
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "CheckWebGpu", "LoadModel", "LoadModel" ])
            , test "says why the model did not download" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.clickButton "Download Bonsai ternary, 2-bit"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (pipelineFailed "Error" "HTTP 404 for model_index.json")
                        |> ProgramTest.ensureViewHas [ text "HTTP 404 for model_index.json" ]
                        |> ProgramTest.clickButton "Download Bonsai ternary, 2-bit"
                        |> ProgramTest.expectViewHas
                            [ attribute (Html.Attributes.attribute "data-slot" "toast")
                            , attribute (Html.Attributes.attribute "data-ending-style" "")
                            , containing [ text "HTTP 404 for model_index.json" ]
                            ]
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
                        |> ProgramTest.expectViewHasNot
                            [ attribute (Html.Attributes.attribute "data-slot" "badge")
                            , containing [ text "Downloaded" ]
                            ]
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
                        |> ProgramTest.ensureViewHas [ text "Downloaded models removed" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            localStorageWrite
                            (Expect.equal [ { tag = "WriteToLocalStorage", key = "models", value = "[]" } ])
            , test "removes the downloaded copies on a browser without WebGPU" <|
                \() ->
                    startAt "/settings/models" { defaultFlags | models = Just """["prism-ml/bonsai-image-ternary-4B-mlx-2bit"]""" }
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked False)
                        |> ProgramTest.clickButton "Remove downloaded models"
                        |> ProgramTest.ensureViewHas [ tag "button", attribute (Html.Attributes.disabled True), containing [ text "Removing…" ] ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "ModelCacheCleared")
                        |> ProgramTest.ensureViewHas [ text "Downloaded models removed" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "CheckWebGpu", "ClearModelCache", "WriteToLocalStorage" ])
            , test "can't remove the downloaded copies once the worker has failed" <|
                \() ->
                    startAt "/settings/models" { defaultFlags | models = Just """["prism-ml/bonsai-image-ternary-4B-mlx-2bit"]""" }
                        |> ProgramTest.simulateIncomingPort "interopToElm" (pipelineFailed "Error" "Worker unable to start")
                        |> ProgramTest.ensureViewHas [ text "Reload the page to remove them." ]
                        |> ProgramTest.expectViewHas [ tag "button", attribute (Html.Attributes.disabled True), containing [ text "Remove downloaded models" ] ]
            , test "says so when the browser can't run them" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.ensureViewHas [ class "invisible", containing [ text "Manage the models that run in your browser. Your prompts stay on this device." ] ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked False)
                        |> ProgramTest.ensureViewHas [ class "invisible", containing [ text "Manage the models that run in your browser. Your prompts stay on this device." ] ]
                        |> ProgramTest.expectViewHasNot [ class "invisible", containing [ text "Cannot generate images on this device. Pixo needs a WebGPU device." ] ]
            , test "says what the models are for once it knows the browser runs them" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.ensureViewHas [ class "invisible", containing [ text "Cannot generate images on this device. Pixo needs a WebGPU device." ] ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.expectViewHasNot [ class "invisible", containing [ text "Manage the models that run in your browser. Your prompts stay on this device." ] ]
            , test "says the models can't run here instead of offering them" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked False)
                        |> ProgramTest.ensureViewHas [ class "invisible", containing [ text "Download once, use in the studio." ] ]
                        |> ProgramTest.ensureViewHas [ class "invisible", containing [ text "Downloads the first time you use AI Style in the studio." ] ]
                        |> ProgramTest.ensureViewHasNot [ class "invisible", containing [ text "Needs a WebGPU device to run." ] ]
                        |> ProgramTest.expectViewHas
                            [ tag "button"
                            , attribute (Html.Attributes.disabled True)
                            , attribute (Html.Attributes.attribute "aria-label" "Download Bonsai ternary, 2-bit")
                            ]
            ]
        , describe "Publish"
            [ test "offers a free account Pro instead of publishing" <|
                \() ->
                    library
                        |> ProgramTest.clickButton "Open a red fox in the snow"
                        |> ProgramTest.expectView
                            (Query.find [ tag "elmcn-dialog", id "picture" ]
                                >> Query.find [ tag "a", containing [ text "Publish with Pro" ] ]
                                >> Query.has [ attribute (Html.Attributes.href "/settings/account") ]
                            )
            , test "asks before publishing, then encodes the picture as a JPEG" <|
                \() ->
                    proLibrary
                        |> inPicture (ProgramTest.clickButton "Publish")
                        |> ProgramTest.ensureViewHas [ text "Publish this picture? Its prompt, settings and tags will be public." ]
                        |> inPicture (ProgramTest.clickButton "Publish")
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.map (Encode.encode 0) Decode.value)
                            (Expect.equal [ """{"tag":"EncodePicture","data":{"label":"publish:1","url":"blob:https://pixo.pics/1","quality":0.9}}""" ])
            , test "can be cancelled at the question" <|
                \() ->
                    proLibrary
                        |> inPicture (ProgramTest.clickButton "Publish")
                        |> inPicture (ProgramTest.clickButton "Cancel")
                        |> ProgramTest.ensureViewHasNot [ text "Publish this picture?" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [])
            , test "asks the server with the picture's settings, then uploads straight to the link, then confirms" <|
                \() ->
                    encoded proLibrary
                        |> ProgramTest.ensureHttpRequest "POST"
                            "/api/pictures"
                            (Test.Http.expectJsonBody Decode.value
                                (Encode.encode 0
                                    >> Expect.equal """{"prompt":"a red fox in the snow","width":512,"height":512,"steps":4,"seed":7,"model":"prism-ml/bonsai-image-ternary-4B-mlx-2bit","bytes":12,"tags":[]}"""
                                )
                            )
                        |> ProgramTest.simulateHttpResponse "POST" "/api/pictures" (response 201 upload)
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.map (Encode.encode 0) Decode.value)
                            (List.drop 1 >> Expect.equal [ sendBlob ])
                        |> ProgramTest.simulateIncomingPort "interopToElm" (blobSent 200)
                        |> ProgramTest.ensureHttpRequest "POST" ("/api/pictures/" ++ publishedId ++ "/published") (\_ -> Expect.pass)
                        |> ProgramTest.simulateHttpResponse "POST" ("/api/pictures/" ++ publishedId ++ "/published") (response 200 published)
                        |> ProgramTest.ensureViewHas [ text "Published" ]
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.map (Encode.encode 0) Decode.value)
                            (Expect.equal [ publishedPatch ("\"" ++ publishedId ++ "\"") ])
                        |> inPicture (ProgramTest.clickButton "Unpublish")
                        |> ProgramTest.expectHttpRequest "DELETE" ("/api/pictures/" ++ publishedId) (\_ -> Expect.pass)
            , test "publishes the picture's tags" <|
                \() ->
                    encoded (proLibraryOf [ storedWithTags [ "fox" ] 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ])
                        |> ProgramTest.expectHttpRequest "POST"
                            "/api/pictures"
                            (Test.Http.expectJsonBody (Decode.field "tags" (Decode.list Decode.string)) (Expect.equal [ "fox" ]))
            , test "says why the server refused the picture, and uploads nothing" <|
                \() ->
                    encoded proLibrary
                        |> ProgramTest.simulateHttpResponse "POST" "/api/pictures" (response 403 """{"error":"Publishing needs Pro."}""")
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "EncodePicture", "IdbRelease" ])
                        |> ProgramTest.ensureViewHas [ text "Couldn't publish the picture" ]
                        |> ProgramTest.expectViewHas [ text "Publishing needs Pro." ]
            , test "cancels the row and says so when the upload is refused" <|
                \() ->
                    encoded proLibrary
                        |> ProgramTest.simulateHttpResponse "POST" "/api/pictures" (response 201 upload)
                        |> ProgramTest.simulateIncomingPort "interopToElm" (blobSent 403)
                        |> ProgramTest.ensureViewHas [ text "Couldn't publish the picture" ]
                        |> ProgramTest.ensureViewHas [ text "The upload didn't go through." ]
                        |> ProgramTest.expectHttpRequest "DELETE" ("/api/pictures/" ++ publishedId) (\_ -> Expect.pass)
            , test "says why the browser could not encode the picture, and asks the server nothing" <|
                \() ->
                    proLibrary
                        |> inPicture (ProgramTest.clickButton "Publish")
                        |> inPicture (ProgramTest.clickButton "Publish")
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (Encode.object [ ( "tag", Encode.string "PictureEncodeFailed" ), ( "label", Encode.string "publish:1" ), ( "message", Encode.string "The picture is too large." ) ])
                        |> ProgramTest.ensureViewHas [ text "Couldn't publish the picture" ]
                        |> ProgramTest.ensureViewHas [ text "The picture is too large." ]
                        |> ProgramTest.expectHttpRequests "POST" "/api/pictures" (List.length >> Expect.equal 0)
            , test "unpublishes a published picture, and forgets its id" <|
                \() ->
                    proLibraryOf [ storedPublished 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ]
                        |> inPicture (ProgramTest.clickButton "Unpublish")
                        |> ProgramTest.ensureHttpRequest "DELETE" ("/api/pictures/" ++ publishedId) (\_ -> Expect.pass)
                        |> ProgramTest.simulateHttpResponse "DELETE" ("/api/pictures/" ++ publishedId) (response 200 "{}")
                        |> ProgramTest.ensureViewHas [ text "Unpublished" ]
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.map (Encode.encode 0) Decode.value)
                            (Expect.equal [ publishedPatch "null" ])
                        |> ProgramTest.expectViewHas [ tag "button", containing [ text "Publish" ] ]
            ]
        , describe "Library"
            [ test "asks the browser for the saved pictures" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.map (Encode.encode 0) Decode.value)
                            (Expect.equal
                                [ "{\"tag\":\"IdbGetAll\",\"data\":{\"database\":{\"name\":\"pixo\",\"version\":1,\"stores\":[{\"name\":\"pictures\",\"keyPath\":\"id\",\"autoIncrement\":true}]},\"store\":\"pictures\",\"label\":\"\"}}" ]
                            )
            , test "shows every picture in its own shape" <|
                \() ->
                    library
                        |> ProgramTest.ensureViewHas [ attribute (Html.Attributes.style "aspect-ratio" "1024 / 576") ]
                        |> ProgramTest.expectViewHas [ tag "img", attribute (Html.Attributes.src "blob:https://pixo.pics/3") ]
            , test "says so when nothing is saved yet" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateIncomingPort "interopToElm" (libraryLoaded [])
                        |> ProgramTest.ensureViewHas [ text "No pictures yet" ]
                        |> ProgramTest.clickLink "Open the studio" "/studio"
                        |> ProgramTest.expectViewHas [ text "Nothing here yet" ]
            , test "says why the pictures could not be read" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateIncomingPort "interopToElm" (libraryFailed "The database is blocked.")
                        |> ProgramTest.ensureViewHasNot [ text "No pictures yet" ]
                        |> ProgramTest.ensureViewHas [ text "Can't open the library" ]
                        |> ProgramTest.expectViewHas [ text "The database is blocked." ]
            , test "keeps the skeleton and reads the pictures again on request" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateIncomingPort "interopToElm" (libraryFailed "The database is blocked.")
                        |> inToast "Can't open the library" (ProgramTest.clickButton "Try again")
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "IdbGetAll", "IdbGetAll" ])
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ])
                        |> ProgramTest.expectViewHas [ tag "img", attribute (Html.Attributes.src "blob:https://pixo.pics/1") ]
            , test "shows a toast over the open picture" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ])
                        |> ProgramTest.clickButton "Open a red fox in the snow"
                        |> ProgramTest.within (Query.find [ tag "elmcn-dialog", id "picture" ]) (ProgramTest.clickButton "Copy picture")
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "PictureCopied")
                        |> ProgramTest.expectView
                            (Expect.all
                                [ Query.find [ tag "elmcn-dialog", id "picture" ]
                                    >> Query.has [ attribute (Html.Attributes.attribute "data-slot" "toast-viewport") ]
                                , Query.findAll [ attribute (Html.Attributes.attribute "data-slot" "toast-viewport") ]
                                    >> Query.count (Expect.equal 1)
                                ]
                            )
            , test "finds pictures by their prompt" <|
                \() ->
                    library
                        |> ProgramTest.fillIn "search" "Search prompts" "sunset light"
                        |> ProgramTest.ensureViewHas [ attribute (Html.Attributes.attribute "aria-label" "Open a city skyline at sunset") ]
                        |> ProgramTest.ensureViewHas [ tag "img", attribute (Html.Attributes.src "blob:https://pixo.pics/2") ]
                        |> ProgramTest.expectViewHasNot [ tag "img", attribute (Html.Attributes.src "blob:https://pixo.pics/1") ]
            , test "finds a prompt whatever accents the search is typed with" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded
                                [ stored 1 "blob:https://pixo.pics/1" "the old town of krakow at dawn" 512 512
                                , stored 2 "blob:https://pixo.pics/2" "a bowl of ramen" 512 512
                                ]
                            )
                        |> ProgramTest.fillIn "search" "Search prompts" "Kraków"
                        |> ProgramTest.ensureViewHasNot [ attribute (Html.Attributes.attribute "aria-label" "Open a bowl of ramen") ]
                        |> ProgramTest.expectViewHas [ tag "img", attribute (Html.Attributes.src "blob:https://pixo.pics/1") ]
            , test "says so when no prompt matches" <|
                \() ->
                    library
                        |> ProgramTest.fillIn "search" "Search prompts" "submarine"
                        |> ProgramTest.expectViewHas [ text "Nothing matches “submarine”" ]
            , test "lists the library's tags with their counts" <|
                \() ->
                    taggedLibrary
                        |> ProgramTest.ensureViewHas [ tag "button", containing [ text "All pictures" ] ]
                        |> ProgramTest.ensureViewHas [ tag "button", containing [ text "sunset" ] ]
                        |> ProgramTest.ensureViewHas [ tag "button", containing [ text "fox" ] ]
                        |> ProgramTest.expectViewHas
                            [ attribute (Html.Attributes.attribute "data-sidebar" "menu-badge")
                            , text "2"
                            ]
            , test "shows each tag with its newest picture" <|
                \() ->
                    taggedLibrary
                        |> ProgramTest.expectView
                            (Query.find [ tag "button", containing [ text "sunset" ] ]
                                >> Query.find [ tag "img" ]
                                >> Query.has [ attribute (Html.Attributes.src "blob:https://pixo.pics/3") ]
                            )
            , test "folds the least used tags past the first eight" <|
                \() ->
                    manyTagsLibrary
                        |> ProgramTest.ensureView (Query.find [ tag "li", containing [ text "valley" ] ] >> Query.has [ class "md:hidden" ])
                        |> ProgramTest.ensureView (Query.find [ tag "li", containing [ text "lake" ] ] >> Query.hasNot [ class "md:hidden" ])
                        |> ProgramTest.expectViewHas [ tag "button", containing [ text "Show 3 more" ] ]
            , test "shows every tag once asked" <|
                \() ->
                    manyTagsLibrary
                        |> ProgramTest.clickButton "Show 3 more"
                        |> ProgramTest.ensureView (Query.find [ tag "li", containing [ text "valley" ] ] >> Query.hasNot [ class "md:hidden" ])
                        |> ProgramTest.clickButton "Show fewer"
                        |> ProgramTest.expectView (Query.find [ tag "li", containing [ text "valley" ] ] >> Query.has [ class "md:hidden" ])
            , test "keeps the chosen tag in view while the rest is folded" <|
                \() ->
                    manyTagsLibrary
                        |> ProgramTest.clickButton "valley"
                        |> ProgramTest.ensureView (Query.find [ tag "li", containing [ text "valley" ] ] >> Query.hasNot [ class "md:hidden" ])
                        |> ProgramTest.expectViewHas [ tag "button", containing [ text "Show 2 more" ] ]
            , test "folds nothing with ten tags or fewer" <|
                \() ->
                    taggedLibrary
                        |> ProgramTest.expectViewHasNot [ tag "button", containing [ text "Show " ] ]
            , test "shows only the pictures with the tag chosen" <|
                \() ->
                    taggedLibrary
                        |> ProgramTest.clickButton "sunset"
                        |> ProgramTest.ensureViewHas [ attribute (Html.Attributes.attribute "aria-label" "Open a city skyline at sunset") ]
                        |> ProgramTest.ensureViewHasNot [ attribute (Html.Attributes.attribute "aria-label" "Open a red fox in the snow") ]
                        |> ProgramTest.clickButton "All pictures"
                        |> ProgramTest.expectViewHas [ attribute (Html.Attributes.attribute "aria-label" "Open a red fox in the snow") ]
            , test "says how pictures get tags when none has any" <|
                \() ->
                    library
                        |> ProgramTest.expectViewHas [ text "No tags yet. The prompt writer tags pictures as you save them." ]
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
            , test "shows the next or the last picture with the arrow keys" <|
                \() ->
                    library
                        |> ProgramTest.clickButton "Open a lighthouse on a cliff at sunset"
                        |> ProgramTest.simulateDomEvent
                            (Query.find [ attribute (Html.Attributes.attribute "data-slot" "lightbox") ])
                            ( "keydown", Encode.object [ ( "key", Encode.string "ArrowRight" ) ] )
                        |> ProgramTest.ensureViewHas
                            [ id "picture", containing [ tag "img", attribute (Html.Attributes.alt "a red fox in the snow") ] ]
                        |> ProgramTest.simulateDomEvent
                            (Query.find [ attribute (Html.Attributes.attribute "data-slot" "lightbox") ])
                            ( "keydown", Encode.object [ ( "key", Encode.string "ArrowLeft" ) ] )
                        |> ProgramTest.simulateDomEvent
                            (Query.find [ attribute (Html.Attributes.attribute "data-slot" "lightbox") ])
                            ( "keydown", Encode.object [ ( "key", Encode.string "ArrowLeft" ) ] )
                        |> ProgramTest.expectViewHas
                            [ id "picture", containing [ tag "img", attribute (Html.Attributes.alt "a city skyline at sunset") ] ]
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
                        |> ProgramTest.simulateIncomingPort "interopToElm" (pictureDeleted 3)
                        |> ProgramTest.ensureViewHasNot [ tag "elmcn-dialog", attribute (Html.Attributes.attribute "open" "") ]
                        |> ProgramTest.expectViewHasNot [ attribute (Html.Attributes.attribute "aria-label" "Open a city skyline at sunset") ]
            , test "deletes a picture once that is confirmed" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ])
                        |> ProgramTest.clickButton "Delete"
                        |> ProgramTest.ensureViewHas [ text "Delete this picture?" ]
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "IdbGetAll" ])
                        |> ProgramTest.clickButton "Delete"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.map3 (\name store key -> ( name, store, key ))
                                (Decode.field "tag" Decode.string)
                                (Decode.at [ "data", "store" ] Decode.string)
                                (Decode.at [ "data", "key" ] Decode.int)
                            )
                            (Expect.equal [ ( "IdbDelete", "pictures", 1 ) ])
                        |> ProgramTest.simulateIncomingPort "interopToElm" (pictureDeleted 1)
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.map2 Tuple.pair
                                (Decode.field "tag" Decode.string)
                                (Decode.at [ "data", "urls" ] (Decode.list Decode.string))
                            )
                            (Expect.equal [ ( "IdbRelease", [ "blob:https://pixo.pics/1" ] ) ])
                        |> ProgramTest.expectViewHas [ text "No pictures yet" ]
            , test "keeps a picture when its deletion is cancelled" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ])
                        |> ProgramTest.clickButton "Delete"
                        |> ProgramTest.clickButton "Cancel"
                        |> ProgramTest.ensureViewHasNot [ text "Delete this picture?" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "IdbGetAll" ])
            , test "copies a picture and opens the network to post it on" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ])
                        |> ProgramTest.clickButton "Bluesky"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.map2 Tuple.pair
                                (Decode.field "tag" Decode.string)
                                (Decode.maybe (Decode.at [ "data", "url" ] Decode.string))
                            )
                            (Expect.equal
                                [ ( "IdbGetAll", Nothing )
                                , ( "CopyPicture", Just "blob:https://pixo.pics/1" )
                                , ( "OpenTab", Just "https://bsky.app/intent/compose?text=a%20red%20fox%20in%20the%20snow" )
                                ]
                            )
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "PictureCopied")
                        |> ProgramTest.ensureViewHas [ text "Picture copied" ]
                        |> ProgramTest.clickButton "Close toast"
                        |> ProgramTest.expectViewHas
                            [ attribute (Html.Attributes.attribute "data-slot" "toast")
                            , attribute (Html.Attributes.attribute "data-ending-style" "")
                            , containing [ text "Picture copied" ]
                            ]
            , test "says so when the browser won't hand the picture over" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ])
                        |> ProgramTest.clickButton "Copy picture"
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (Encode.object [ ( "tag", Encode.string "ShareFailed" ), ( "name", Encode.string "NotAllowedError" ) ])
                        |> ProgramTest.ensureViewHas [ text "Couldn't share the picture" ]
                        |> ProgramTest.expectViewHas [ text "Download it instead." ]
            , test "offers the device's share sheet where there is one" <|
                \() ->
                    startAt "/library" { defaultFlags | canShare = True }
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
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
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ])
                        |> ProgramTest.expectViewHasNot [ text "Share…" ]
            , test "asks to log in when signed out" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.ensureViewHas [ text "Your library" ]
                        |> ProgramTest.ensureViewHas [ tag "a", attribute (Html.Attributes.href "/login"), containing [ text "Log in" ] ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [])
            , test "loads the pictures once the session is known" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [])
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "IdbGetAll" ])
            , test "hides another account's pictures" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded
                                [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512
                                , storedBy "bob" 2 "blob:https://pixo.pics/2" "a lighthouse on a cliff at sunset" 1024 576
                                ]
                            )
                        |> ProgramTest.ensureViewHas [ tag "img", attribute (Html.Attributes.src "blob:https://pixo.pics/1") ]
                        |> ProgramTest.expectViewHasNot [ tag "img", attribute (Html.Attributes.src "blob:https://pixo.pics/2") ]
            , test "claims pictures saved before accounts" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (libraryLoaded [ unowned 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ])
                        |> ProgramTest.ensureViewHas [ tag "img", attribute (Html.Attributes.src "blob:https://pixo.pics/1") ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.maybe
                                (Decode.map3 (\name label fields -> ( name, label, Encode.encode 0 fields ))
                                    (Decode.field "tag" Decode.string)
                                    (Decode.at [ "data", "label" ] Decode.string)
                                    (Decode.at [ "data", "fields" ] Decode.value)
                                )
                            )
                            (List.filterMap identity
                                >> List.filter (\( name, _, _ ) -> name == "IdbPatch")
                                >> Expect.equal [ ( "IdbPatch", "owner", "{\"owner\":\"ada\"}" ) ]
                            )
            , test "forgets the pictures on sign-out" <|
                \() ->
                    library
                        |> ProgramTest.clickButton "Sign out"
                        |> ProgramTest.simulateHttpOk "DELETE" "/api/auth/session" "{}"
                        |> ProgramTest.clickLink "Library" "/library"
                        |> ProgramTest.ensureViewHas [ text "Your library" ]
                        |> ProgramTest.ensureViewHasNot [ tag "img", attribute (Html.Attributes.src "blob:https://pixo.pics/1") ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (List.member "IdbRelease" >> Expect.equal True)
            ]
        ]


type alias Flags =
    { theme : Maybe String
    , systemDark : Bool
    , entropy : Int
    , models : Maybe String
    , stylist : Maybe String
    , canShare : Bool
    , sidebar : Maybe String
    , webGpu : Bool
    }


defaultFlags : Flags
defaultFlags =
    { theme = Nothing, systemDark = False, entropy = 42, models = Nothing, stylist = Nothing, canShare = False, sidebar = Nothing, webGpu = True }


ready : ProgramTest Main.Model Main.Msg (Effect Main.Msg)
ready =
    start defaultFlags
        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
        |> ProgramTest.clickLink "Settings" "/settings/general"
        |> ProgramTest.clickLink "Models" "/settings/models"
        |> ProgramTest.clickButton "Download Bonsai ternary, 2-bit"
        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "ModelLoaded")
        |> ProgramTest.clickLink "Studio" "/studio"
        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
            (Decode.field "tag" Decode.string)
            (Expect.equal [ "CheckWebGpu", "LoadModel", "WriteToLocalStorage" ])


readyToTag : ProgramTest Main.Model Main.Msg (Effect Main.Msg)
readyToTag =
    start { defaultFlags | stylist = Just "Qwen3-1.7B-q4f16_1-MLC" }
        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
        |> ProgramTest.clickLink "Settings" "/settings/general"
        |> ProgramTest.clickLink "Models" "/settings/models"
        |> ProgramTest.clickButton "Download Bonsai ternary, 2-bit"
        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "ModelLoaded")
        |> ProgramTest.clickLink "Studio" "/studio"
        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
            (Decode.field "tag" Decode.string)
            (Expect.equal [ "CheckWebGpu", "LoadModel", "WriteToLocalStorage" ])


outgoing : Decode.Decoder ( String, Maybe String )
outgoing =
    Decode.map2 Tuple.pair
        (Decode.field "tag" Decode.string)
        (Decode.maybe (Decode.at [ "data", "model" ] Decode.string))


turn : Decode.Decoder ( String, String )
turn =
    Decode.map2 Tuple.pair (Decode.field "role" Decode.string) (Decode.field "content" Decode.string)


lastTurn : List ( String, String ) -> Maybe ( String, String )
lastTurn turns =
    List.head (List.reverse turns)


patchOrRelease : Decode.Decoder String
patchOrRelease =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\name ->
                if name == "IdbPatch" then
                    Decode.map3 (\key label fields -> name ++ ":" ++ key ++ ":" ++ label ++ ":" ++ fields)
                        (Decode.at [ "data", "key" ] (Decode.map String.fromInt Decode.int))
                        (Decode.at [ "data", "label" ] Decode.string)
                        (Decode.at [ "data", "fields", "tags" ] (Decode.value |> Decode.map (Encode.encode 0)))

                else
                    Decode.succeed name
            )


library : ProgramTest Main.Model Main.Msg (Effect Main.Msg)
library =
    startAt "/library" defaultFlags
        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
        |> ProgramTest.simulateIncomingPort "interopToElm"
            (libraryLoaded
                [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512
                , stored 2 "blob:https://pixo.pics/2" "a lighthouse on a cliff at sunset" 1024 576
                , stored 3 "blob:https://pixo.pics/3" "a city skyline at sunset" 576 1024
                ]
            )


olderPictures : Encode.Value
olderPictures =
    libraryLoaded
        [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512
        , stored 2 "blob:https://pixo.pics/2" "a lighthouse on a cliff at sunset" 1024 576
        , stored 3 "blob:https://pixo.pics/3" "a city skyline at sunset" 576 1024
        ]


manyTagsLibrary : ProgramTest Main.Model Main.Msg (Effect Main.Msg)
manyTagsLibrary =
    startAt "/library" defaultFlags
        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
        |> ProgramTest.simulateIncomingPort "interopToElm"
            (libraryLoaded
                [ storedWithTags [ "autumn", "beach", "cabin", "desert", "forest", "harbor", "island", "lake", "meadow", "river", "valley" ] 1 "blob:https://pixo.pics/1" "a valley in autumn" 512 512
                ]
            )


taggedLibrary : ProgramTest Main.Model Main.Msg (Effect Main.Msg)
taggedLibrary =
    startAt "/library" defaultFlags
        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" me
        |> ProgramTest.simulateIncomingPort "interopToElm"
            (libraryLoaded
                [ storedWithTags [ "fox" ] 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512
                , storedWithTags [ "sunset", "lighthouse" ] 2 "blob:https://pixo.pics/2" "a lighthouse on a cliff at sunset" 1024 576
                , storedWithTags [ "sunset" ] 3 "blob:https://pixo.pics/3" "a city skyline at sunset" 576 1024
                ]
            )


publishedId : String
publishedId =
    "6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11"


upload : String
upload =
    """{"id":"6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11","url":"https://acc.r2.cloudflarestorage.com/uploads/incoming/6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11.jpg?X-Amz-Signature=x","headers":[["content-type","image/jpeg"]]}"""


published : String
published =
    """{"id":"6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11","url":"https://img.pixo.pics/6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11.jpg","publishedAt":"2026-11-17T10:00:00Z"}"""


sendBlob : String
sendBlob =
    """{"tag":"SendBlob","data":{"label":"publish:1","url":"blob:https://pixo.pics/jpeg-1","method":"PUT","endpoint":"https://acc.r2.cloudflarestorage.com/uploads/incoming/6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11.jpg?X-Amz-Signature=x","headers":[["content-type","image/jpeg"]],"release":true}}"""


publishedPatch : String -> String
publishedPatch value =
    """{"tag":"IdbPatch","data":{"database":{"name":"pixo","version":1,"stores":[{"name":"pictures","keyPath":"id","autoIncrement":true}]},"store":"pictures","key":1,"fields":{"published":""" ++ value ++ """},"label":"published"}}"""


blobSent : Int -> Encode.Value
blobSent status =
    Encode.object
        [ ( "tag", Encode.string "BlobSent" )
        , ( "label", Encode.string "publish:1" )
        , ( "status", Encode.int status )
        , ( "body", Encode.string "" )
        ]


storedPublished : Int -> String -> String -> Int -> Int -> Encode.Value
storedPublished id url prompt width height =
    Encode.object (storedFields id url prompt width height ++ [ ( "owner", Encode.string "ada" ), ( "published", Encode.string publishedId ) ])


proLibrary : ProgramTest Main.Model Main.Msg (Effect Main.Msg)
proLibrary =
    proLibraryOf [ stored 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512 ]


proLibraryOf : List Encode.Value -> ProgramTest Main.Model Main.Msg (Effect Main.Msg)
proLibraryOf pictures =
    startAt "/library" defaultFlags
        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" mePro
        |> ProgramTest.simulateIncomingPort "interopToElm" (libraryLoaded pictures)
        |> ProgramTest.clickButton "Open a red fox in the snow"
        |> ProgramTest.ensureOutgoingPortValues "interopFromElm" Decode.value (\_ -> Expect.pass)


feedIdA : String
feedIdA =
    "6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11"


feedIdB : String
feedIdB =
    "0a1b2c3d-4e5f-4a6b-8c7d-9e0f1a2b3c4d"


feedPicture : String -> String -> String -> List String -> String
feedPicture id username prompt tags =
    """{"id":"""
        ++ quoted id
        ++ ""","url":"https://img.pixo.pics/"""
        ++ id
        ++ """.jpg","username":"""
        ++ quoted username
        ++ ""","prompt":"""
        ++ quoted prompt
        ++ ""","width":512,"height":512,"steps":4,"seed":7,"model":"m","tags":["""
        ++ String.join "," (List.map quoted tags)
        ++ """],"publishedAt":"2026-11-17T10:00:00Z"}"""


feedPage : List String -> Maybe String -> String
feedPage pictures next =
    """{"pictures":["""
        ++ String.join "," pictures
        ++ """],"next":"""
        ++ Maybe.withDefault "null" (Maybe.map quoted next)
        ++ "}"


profilePage : String -> List String -> Maybe String -> String
profilePage username pictures next =
    """{"username":""" ++ quoted username ++ ""","pictures":[""" ++ String.join "," pictures ++ """],"next":""" ++ Maybe.withDefault "null" (Maybe.map quoted next) ++ "}"


quoted : String -> String
quoted value =
    Encode.encode 0 (Encode.string value)


exploring : ProgramTest Main.Model Main.Msg (Effect Main.Msg)
exploring =
    startAt "/" defaultFlags
        |> ProgramTest.simulateHttpOk "GET" "/api/feed" (feedPage [ feedPicture feedIdA "ada" "a red fox" [ "fox", "snow" ] ] (Just feedIdA))
        |> ProgramTest.simulateHttpOk "GET" "/api/feed/tags" """[{"tag":"fox","pictures":2}]"""


inFeedPicture : (ProgramTest Main.Model Main.Msg (Effect Main.Msg) -> ProgramTest Main.Model Main.Msg (Effect Main.Msg)) -> ProgramTest Main.Model Main.Msg (Effect Main.Msg) -> ProgramTest Main.Model Main.Msg (Effect Main.Msg)
inFeedPicture =
    ProgramTest.within (Query.find [ tag "elmcn-dialog", id "feed-picture" ])


inProfilePicture : (ProgramTest Main.Model Main.Msg (Effect Main.Msg) -> ProgramTest Main.Model Main.Msg (Effect Main.Msg)) -> ProgramTest Main.Model Main.Msg (Effect Main.Msg) -> ProgramTest Main.Model Main.Msg (Effect Main.Msg)
inProfilePicture =
    ProgramTest.within (Query.find [ tag "elmcn-dialog", id "profile-picture" ])


inPicture : (ProgramTest Main.Model Main.Msg (Effect Main.Msg) -> ProgramTest Main.Model Main.Msg (Effect Main.Msg)) -> ProgramTest Main.Model Main.Msg (Effect Main.Msg) -> ProgramTest Main.Model Main.Msg (Effect Main.Msg)
inPicture =
    ProgramTest.within (Query.find [ tag "elmcn-dialog", id "picture" ])


encoded : ProgramTest Main.Model Main.Msg (Effect Main.Msg) -> ProgramTest Main.Model Main.Msg (Effect Main.Msg)
encoded app =
    app
        |> inPicture (ProgramTest.clickButton "Publish")
        |> inPicture (ProgramTest.clickButton "Publish")
        |> ProgramTest.simulateIncomingPort "interopToElm"
            (Encode.object
                [ ( "tag", Encode.string "PictureEncoded" )
                , ( "label", Encode.string "publish:1" )
                , ( "url", Encode.string "blob:https://pixo.pics/jpeg-1" )
                , ( "bytes", Encode.int 12 )
                ]
            )


start : Flags -> ProgramTest Main.Model Main.Msg (Effect Main.Msg)
start =
    startAt "/studio"


startAt : String -> Flags -> ProgramTest Main.Model Main.Msg (Effect Main.Msg)
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
                , ( "stylist", Maybe.withDefault Encode.null (Maybe.map Encode.string flags.stylist) )
                , ( "canShare", Encode.bool flags.canShare )
                , ( "sidebar", Maybe.withDefault Encode.null (Maybe.map Encode.string flags.sidebar) )
                , ( "webGpu", Encode.bool flags.webGpu )
                ]
            )


simulateEffects : Effect Main.Msg -> ProgramTest.SimulatedEffect Main.Msg
simulateEffects effect =
    case effect of
        Effect.NoEffect ->
            SimulatedEffect.Cmd.none

        Effect.Batch effects ->
            SimulatedEffect.Cmd.batch (List.map simulateEffects effects)

        Effect.Request { method, headers, url, body, onResponse } ->
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

        Effect.SendToJs value ->
            SimulatedEffect.Ports.send "interopFromElm"
                (TsEncode.encoder Interop.interop.fromElm value)

        Effect.PushUrl url ->
            SimulatedEffect.Navigation.pushUrl url

        Effect.Load url ->
            SimulatedEffect.Navigation.load url

        Effect.ReadTime toMsg ->
            SimulatedEffect.Task.perform toMsg SimulatedEffect.Time.now

        Effect.ScrollTo _ ->
            SimulatedEffect.Cmd.none

        Effect.Notify notice ->
            SimulatedEffect.Task.perform Main.GotToast (SimulatedEffect.Task.succeed (Toast.add (Notice.toToast notice)))

        Effect.Dismiss id ->
            SimulatedEffect.Task.perform Main.GotToast (SimulatedEffect.Task.succeed (Toast.close id))

        Effect.Sleep milliseconds msg ->
            SimulatedEffect.Process.sleep milliseconds
                |> SimulatedEffect.Task.perform (\() -> msg)

        Effect.Toaster _ ->
            SimulatedEffect.Cmd.none


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


throttled : Int -> Http.Response String
throttled seconds =
    Test.Http.httpResponse
        { statusCode = 429
        , headers = [ ( "retry-after", String.fromInt seconds ) ]
        , body = """{"error":"Too many attempts."}"""
        }


inToast : String -> (ProgramTest Main.Model Main.Msg (Effect Main.Msg) -> ProgramTest Main.Model Main.Msg (Effect Main.Msg)) -> ProgramTest Main.Model Main.Msg (Effect Main.Msg) -> ProgramTest Main.Model Main.Msg (Effect Main.Msg)
inToast heading =
    ProgramTest.within
        (Query.find
            [ attribute (Html.Attributes.attribute "data-slot" "toast")
            , containing [ text heading ]
            ]
        )


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
    { tag : String
    , store : String
    , blobs : List ( String, String )
    , label : String
    , prompt : String
    , width : Int
    , height : Int
    , steps : Int
    , seed : Int
    , model : String
    }


pictureSave : Decode.Decoder PictureSave
pictureSave =
    Decode.map4 PictureSave
        (Decode.field "tag" Decode.string)
        (Decode.at [ "data", "store" ] Decode.string)
        (Decode.at [ "data", "blobs" ]
            (Decode.list (Decode.map2 Tuple.pair (Decode.field "field" Decode.string) (Decode.field "url" Decode.string)))
        )
        (Decode.at [ "data", "label" ] Decode.string)
        |> Decode.andThen
            (\save ->
                Decode.at [ "data", "row" ]
                    (Decode.map6 save
                        (Decode.field "prompt" Decode.string)
                        (Decode.field "width" Decode.int)
                        (Decode.field "height" Decode.int)
                        (Decode.field "steps" Decode.int)
                        (Decode.field "seed" Decode.int)
                        (Decode.field "model" Decode.string)
                    )
            )


stored : Int -> String -> String -> Int -> Int -> Encode.Value
stored =
    storedBy "ada"


storedFields : Int -> String -> String -> Int -> Int -> List ( String, Encode.Value )
storedFields id url prompt width height =
    [ ( "id", Encode.int id )
    , ( "blob", Encode.string url )
    , ( "prompt", Encode.string prompt )
    , ( "width", Encode.int width )
    , ( "height", Encode.int height )
    , ( "steps", Encode.int 4 )
    , ( "seed", Encode.int 7 )
    , ( "model", Encode.string "prism-ml/bonsai-image-ternary-4B-mlx-2bit" )
    ]


storedBy : String -> Int -> String -> String -> Int -> Int -> Encode.Value
storedBy name id url prompt width height =
    Encode.object (storedFields id url prompt width height ++ [ ( "owner", Encode.string name ) ])


unowned : Int -> String -> String -> Int -> Int -> Encode.Value
unowned id url prompt width height =
    Encode.object (storedFields id url prompt width height)


storedWithTags : List String -> Int -> String -> String -> Int -> Int -> Encode.Value
storedWithTags names id url prompt width height =
    Encode.object
        [ ( "id", Encode.int id )
        , ( "blob", Encode.string url )
        , ( "prompt", Encode.string prompt )
        , ( "width", Encode.int width )
        , ( "height", Encode.int height )
        , ( "steps", Encode.int 4 )
        , ( "seed", Encode.int 7 )
        , ( "model", Encode.string "prism-ml/bonsai-image-ternary-4B-mlx-2bit" )
        , ( "tags", Encode.list Encode.string names )
        , ( "owner", Encode.string "ada" )
        ]


libraryLoaded : List Encode.Value -> Encode.Value
libraryLoaded pictures =
    Encode.object
        [ ( "tag", Encode.string "IdbRows" )
        , ( "label", Encode.string "" )
        , ( "rows", Encode.list identity pictures )
        ]


pictureSaved : String -> Encode.Value -> Encode.Value
pictureSaved source picture =
    Encode.object
        [ ( "tag", Encode.string "IdbAdded" )
        , ( "label", Encode.string source )
        , ( "row", picture )
        ]


pictureDeleted : Int -> Encode.Value
pictureDeleted id =
    Encode.object
        [ ( "tag", Encode.string "IdbDeleted" )
        , ( "label", Encode.string "" )
        , ( "key", Encode.int id )
        ]


libraryFailed : String -> Encode.Value
libraryFailed message =
    Encode.object
        [ ( "tag", Encode.string "IdbFailed" )
        , ( "label", Encode.string "" )
        , ( "message", Encode.string message )
        ]


tagged : String -> Encode.Value
tagged name =
    Encode.object [ ( "tag", Encode.string name ) ]


webGpuChecked : Bool -> Encode.Value
webGpuChecked supported =
    if supported then
        adapterFound [ "shader-f16" ]

    else
        Encode.object
            [ ( "tag", Encode.string "WebGpuChecked" )
            , ( "features", Encode.null )
            ]


adapterFound : List String -> Encode.Value
adapterFound features =
    Encode.object
        [ ( "tag", Encode.string "WebGpuChecked" )
        , ( "features", Encode.list Encode.string features )
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


me : String
me =
    """{"email":"ada@example.com","username":"ada","billing":{"plan":"free","renewsAt":null,"endsAt":null,"pastDue":false,"canManage":false}}"""


mePro : String
mePro =
    """{"email":"ada@example.com","username":"ada","billing":{"plan":"pro","renewsAt":"2026-12-17T10:00:00Z","endsAt":null,"pastDue":false,"canManage":true}}"""


meEnding : String
meEnding =
    """{"email":"ada@example.com","username":"ada","billing":{"plan":"pro","renewsAt":null,"endsAt":"2026-12-17T10:00:00Z","pastDue":false,"canManage":true}}"""


mePastDue : String
mePastDue =
    """{"email":"ada@example.com","username":"ada","billing":{"plan":"pro","renewsAt":"2026-12-17T10:00:00Z","endsAt":null,"pastDue":true,"canManage":true}}"""


checkout : String
checkout =
    """{"transactionId":"txn_01","clientToken":"test_token","environment":"sandbox"}"""


checkoutEvent : String -> Encode.Value
checkoutEvent name =
    Encode.object [ ( "tag", Encode.string "CheckoutEvent" ), ( "name", Encode.string name ) ]


account : String
account =
    """{"email":"ada@example.com","username":"ada"}"""


health : String
health =
    """{"status":"pass","releaseId":"local","region":"local","checks":{"database":{"status":"pass","latencyMs":3}}}"""


notSignedIn : Http.Response String
notSignedIn =
    response 401 """{"error":"Not signed in."}"""


oldAccount : String
oldAccount =
    """{"email":"ada@example.com","username":"ada","newAccount":false}"""


newAccount : String
newAccount =
    """{"email":"ada@example.com","username":"ada","newAccount":true}"""


styled : ProgramTest Main.Model Main.Msg (Effect Main.Msg)
styled =
    start defaultFlags
        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
        |> ProgramTest.fillIn "prompt" "Prompt" "a fox in the snow"
        |> ProgramTest.clickButton "Photo"
        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "StylistLoaded")
        |> ProgramTest.simulateIncomingPort "interopToElm" (promptWritten foxPrompt)
        |> ProgramTest.ensureViewHas [ id "prompt", attribute (Html.Attributes.value foxPrompt) ]
        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
            (Decode.field "tag" Decode.string)
            (Expect.equal [ "CheckWebGpu", "LoadStylist", "WritePrompt", "WriteToLocalStorage" ])


foxPrompt : String
foxPrompt =
    String.join "\n"
        [ "Subject: a red fox curled up in fresh snow"
        , "Style: realistic photograph, natural colors, fine detail"
        , "Setting: a quiet birch forest"
        , "Lighting: soft blue-hour light"
        , "Camera: shot on Canon EOS R5 with a 400mm f/2.8 lens"
        , "Mood: calm, hushed"
        ]


type alias Writing =
    { model : String
    , roles : List String
    , idea : String
    , thinking : Bool
    }


writing : Decode.Decoder (Maybe Writing)
writing =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\name ->
                if name == "WritePrompt" then
                    Decode.map Just
                        (Decode.field "data"
                            (Decode.map4 Writing
                                (Decode.field "model" Decode.string)
                                (Decode.field "messages" (Decode.list (Decode.field "role" Decode.string)))
                                (Decode.field "messages" (Decode.list (Decode.field "content" Decode.string))
                                    |> Decode.map (List.reverse >> List.head >> Maybe.withDefault "")
                                )
                                (Decode.field "thinking" Decode.bool)
                            )
                        )

                else
                    Decode.succeed Nothing
            )


lockedBut : Selector.Selector -> Query.Single msg -> Expect.Expectation
lockedBut kept =
    let
        off selectors =
            Query.findAll selectors >> Query.each (Query.has [ attribute (Html.Attributes.disabled True) ])
    in
    Query.find [ tag "form" ]
        >> Expect.all
            [ Query.findAll [ tag "button", kept ] >> Query.count (Expect.equal 1)
            , Query.findAll [ tag "button", kept ] >> Query.each (Query.hasNot [ attribute (Html.Attributes.disabled True) ])
            , Query.find [ id "prompt" ] >> Query.has [ attribute (Html.Attributes.disabled True) ]
            , off [ tag "input" ]
            , off [ id "ai-style" ]
            , off [ id "weights" ]
            , off [ id "size" ]
            , off [ attribute (Html.Attributes.attribute "aria-label" "Undo") ]
            , off [ attribute (Html.Attributes.type_ "submit") ]
            ]


instructing : Decode.Decoder (Maybe String)
instructing =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\name ->
                if name == "WritePrompt" then
                    Decode.map Just (Decode.at [ "data", "messages" ] (Decode.index 0 (Decode.field "content" Decode.string)))

                else
                    Decode.succeed Nothing
            )


stylistProgress : Float -> Encode.Value
stylistProgress progress =
    Encode.object
        [ ( "tag", Encode.string "StylistProgress" )
        , ( "progress", Encode.float progress )
        ]


promptStreamed : String -> Encode.Value
promptStreamed text =
    Encode.object
        [ ( "tag", Encode.string "PromptStreamed" )
        , ( "text", Encode.string text )
        ]


promptWritten : String -> Encode.Value
promptWritten text =
    Encode.object
        [ ( "tag", Encode.string "PromptWritten" )
        , ( "text", Encode.string text )
        ]


stylistFailed : String -> String -> Encode.Value
stylistFailed name message =
    Encode.object
        [ ( "tag", Encode.string "StylistFailed" )
        , ( "name", Encode.string name )
        , ( "message", Encode.string message )
        ]
