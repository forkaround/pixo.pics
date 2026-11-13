module MainTest exposing (suite)

import Effect exposing (Effect)
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
import SimulatedEffect.Task
import SimulatedEffect.Time
import Stylist
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, containing, id, tag, text)
import Test.Http
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


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
                            (response 429 """{"error":"Too many attempts. Try again later."}""")
                        |> ProgramTest.expectViewHas [ text "Too many attempts. Try again later." ]
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
                            (response 401 """{"error":"This passkey could not be verified. Try again, or sign in with an emailed link."}""")
                        |> ProgramTest.expectViewHas
                            [ text "This passkey could not be verified. Try again, or sign in with an emailed link." ]
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
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.ensureViewHasNot [ text "Log in" ]
                        |> ProgramTest.ensureViewHasNot [ text "Create an account" ]
                        |> ProgramTest.ensureViewHas [ tag "button", containing [ text "ada@example.com" ] ]
                        |> ProgramTest.clickButton "Account settings"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/settings/account")
                        |> ProgramTest.expectViewHas [ text "Signed in as ada (ada@example.com)" ]
            , test "signs out from the account's menu, back to home" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
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
                        |> ProgramTest.ensureViewHas [ text "Can't check who is signed in" ]
                        |> ProgramTest.clickButton "Try again"
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.expectViewHas [ tag "button", containing [ text "ada@example.com" ] ]
            ]
        , describe "Account"
            [ test "deletes the account only after confirmation and returns home signed out" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
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
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
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
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.clickButton "Delete account"
                        |> ProgramTest.clickButton "Cancel"
                        |> ProgramTest.ensureViewHasNot [ tag "elmcn-dialog", attribute (Html.Attributes.attribute "id" "delete-account"), attribute (Html.Attributes.attribute "open" "") ]
                        |> ProgramTest.expectViewHas [ text "Signed in as ada (ada@example.com)" ]
            , test "keeps deletion pending when Escape is pressed" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
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
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.clickButton "Delete account"
                        |> ProgramTest.within (Query.find [ attribute (Html.Attributes.attribute "role" "alertdialog") ]) (ProgramTest.clickButton "Delete")
                        |> ProgramTest.simulateHttpResponse "DELETE"
                            "/api/auth/account"
                            (Test.Http.httpResponse
                                { statusCode = 503, headers = [], body = "{\"error\":\"Please try again.\"}" }
                            )
                        |> ProgramTest.ensureViewHas [ text "Please try again." ]
                        |> ProgramTest.ensureViewHas [ text "Signed in as ada (ada@example.com)" ]
                        |> ProgramTest.within (Query.find [ attribute (Html.Attributes.attribute "role" "alertdialog") ]) (ProgramTest.clickButton "Delete")
                        |> ProgramTest.simulateHttpOk "DELETE" "/api/auth/account" "{}"
                        |> ProgramTest.expectBrowserUrl (Expect.equal "https://pixo.pics/")
            , test "returns to login when the session expired before deletion" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.clickButton "Delete account"
                        |> ProgramTest.within (Query.find [ attribute (Html.Attributes.attribute "role" "alertdialog") ]) (ProgramTest.clickButton "Delete")
                        |> ProgramTest.simulateHttpResponse "DELETE" "/api/auth/account" notSignedIn
                        |> ProgramTest.expectBrowserUrl (Expect.equal "https://pixo.pics/login")
            , test "shows the signed-in account in the subtitle without a welcome card" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.ensureViewHas [ text "Signed in as ada (ada@example.com)" ]
                        |> ProgramTest.ensureViewHasNot [ text "Welcome, ada" ]
                        |> ProgramTest.expectViewHasNot [ text "Signed in on this browser" ]
            , test "sends a signed-out visitor to sign in" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/login")
                        |> ProgramTest.expectViewHas [ text "Use your passkey, or get a link by email." ]
            , test "is listed in the settings" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.clickLink "Account" "/settings/account"
                        |> ProgramTest.expectViewHas [ text "Signed in as ada (ada@example.com)" ]
            , test "is left out of the settings for a visitor who is signed out" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.expectViewHasNot [ text "Account" ]
            , test "signs out, back to home" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
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
                            (response 503 """{"error":"Not available right now. Try again later."}""")
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/settings/account")
                        |> ProgramTest.expectViewHas [ text "Not available right now. Try again later." ]
            , test "checks the session again on request" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET"
                            "/api/auth/session"
                            (response 503 """{"error":"Not available right now. Try again later."}""")
                        |> ProgramTest.simulateHttpResponse "GET"
                            "/api/auth/passkeys"
                            (response 503 """{"error":"Not available right now. Try again later."}""")
                        |> ProgramTest.within (Query.find [ tag "section" ]) (ProgramTest.clickButton "Try again")
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.ensureViewHas [ text "Signed in as ada (ada@example.com)" ]
                        |> ProgramTest.expectViewHas [ text "Apple Passwords" ]
            ]
        , describe "Passkeys"
            [ test "formats the added time once when the passkeys are shown" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
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
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.ensureViewHas [ text "Apple Passwords" ]
                        |> ProgramTest.expectViewHas [ tag "time", attribute (Html.Attributes.attribute "datetime" "2026-11-10T09:30:00Z") ]
            , test "calls a passkey just that when nothing says what made it" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.simulateHttpOk "GET"
                            "/api/auth/passkeys"
                            """[{"id":"oKGio6SlpqeoqaqrrK2urw","name":null,"createdAt":"2026-11-10T09:30:00Z"}]"""
                        |> ProgramTest.expectViewHas [ text "Passkey" ]
            , test "removes a passkey once that is confirmed" <|
                \() ->
                    startAt "/settings/account" defaultFlags
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
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
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
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" ("[" ++ passkey ++ "]")
                        |> ProgramTest.clickButton "Remove Apple Passwords"
                        |> ProgramTest.clickButton "Remove"
                        |> ProgramTest.simulateHttpResponse "DELETE"
                            "/api/auth/passkeys/oKGio6SlpqeoqaqrrK2urw"
                            (response 503 """{"error":"Not available right now. Try again later."}""")
                        |> ProgramTest.ensureViewHas [ text "Not available right now. Try again later." ]
                        |> ProgramTest.expectViewHas [ tag "time", attribute (Html.Attributes.attribute "datetime" "2026-11-10T09:30:00Z") ]
            , test "says so when the account has none" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/passkeys" "[]"
                        |> ProgramTest.expectViewHas [ text "No passkeys yet." ]
            , test "asks the server for options, the browser for a passkey, then saves it" <|
                \() ->
                    startAt "/settings/account" defaultFlags
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
                        |> ProgramTest.expectViewHas [ tag "time", attribute (Html.Attributes.attribute "datetime" "2026-11-10T09:30:00Z") ]
            , test "says so when the passkey is not created" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.clickButton "Add a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.simulateIncomingPort "interopToElm" (passkeyFailed "NotAllowedError")
                        |> ProgramTest.ensureViewHas [ text "The passkey was not created." ]
                        |> ProgramTest.expectHttpRequests "POST" "/api/auth/passkeys" (List.length >> Expect.equal 0)
            , test "says so when the device already has one" <|
                \() ->
                    startAt "/settings/account" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.clickButton "Add a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.simulateIncomingPort "interopToElm" (passkeyFailed "InvalidStateError")
                        |> ProgramTest.expectViewHas [ text "This device already has a passkey for this account." ]
            , test "shows why the server refused the passkey" <|
                \() ->
                    startAt "/settings/account" defaultFlags
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
                    startAt "/settings/account" defaultFlags
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
                        |> ProgramTest.ensureViewHas [ text "The passkey was not created." ]
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
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.clickButton "Create a passkey"
                        |> ProgramTest.simulateHttpOk "POST" "/api/auth/passkeys/options" passkeyOptions
                        |> ProgramTest.simulateIncomingPort "interopToElm" passkeyCreated
                        |> ProgramTest.simulateHttpResponse "POST" "/api/auth/passkeys" (response 201 passkey)
                        |> ProgramTest.expectBrowserUrl (Expect.equal "https://pixo.pics/")
            , test "asks the browser for no passkey once its page is left" <|
                \() ->
                    startAt "/passkey" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
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
                    startAt "/nowhere" defaultFlags
                        |> ProgramTest.expectViewHas [ text "There is nothing at this address." ]
            , test "offers the account at an unknown address when signed in" <|
                \() ->
                    startAt "/nowhere" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.clickLink "Account" "/settings/account"
                        |> ProgramTest.expectViewHas [ text "Signed in as ada (ada@example.com)" ]
            , test "offers to sign in or sign up at an unknown address when signed out" <|
                \() ->
                    startAt "/nowhere" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" notSignedIn
                        |> ProgramTest.ensureViewHas [ text "Create an account" ]
                        |> ProgramTest.clickLink "Log in" "/login"
                        |> ProgramTest.expectViewHas [ text "Use your passkey, or get a link by email." ]
            , test "offers to check the session again at an unknown address" <|
                \() ->
                    startAt "/nowhere" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/auth/session" Test.Http.networkError
                        |> ProgramTest.clickButton "Try again"
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
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
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
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
                        |> ProgramTest.clickLink "Settings" "/settings/models"
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
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
                        |> ProgramTest.clickLink "Start creating" "/studio"
                        |> ProgramTest.ensureBrowserUrl (Expect.equal "https://pixo.pics/studio")
                        |> ProgramTest.expectViewHas [ text "Nothing here yet" ]
            , test "keeps the banner for someone signed in, with a way down to the pictures" <|
                \() ->
                    startAt "/" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/auth/session" account
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
                        |> ProgramTest.ensureViewHasNot [ text "Download and generate" ]
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
                        |> ProgramTest.expectViewHas [ tag "button", attribute (Html.Attributes.disabled False), containing [ text "Download and generate" ] ]
            , test "makes pictures but writes no prompts without 16-bit shaders" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (adapterFound [ "subgroups" ])
                        |> ProgramTest.ensureViewHas [ tag "fieldset", attribute (Html.Attributes.disabled False), containing [ text "Random seed" ] ]
                        |> ProgramTest.ensureViewHas [ tag "button", attribute (Html.Attributes.disabled True), containing [ text "AI Style" ] ]
                        |> ProgramTest.ensureViewHas [ text "Needs 16-bit GPU shaders." ]
                        |> ProgramTest.expectViewHas [ tag "button", attribute (Html.Attributes.disabled False), containing [ text "Download and generate" ] ]
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
                        |> ProgramTest.ensureViewHas [ tag "fieldset", attribute (Html.Attributes.disabled True), containing [ text "Random seed" ] ]
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
                                    [ { tag = "IdbAdd"
                                      , store = "pictures"
                                      , blobs = [ ( "blob", "blob:https://pixo.pics/1" ) ]
                                      , label = "blob:https://pixo.pics/1"
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
                            (Expect.equal [ "IdbGetAll" ])
            , test "tags a saved picture with the prompt model" <|
                \() ->
                    readyToTag
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
            , test "says on the logo that it is tagging" <|
                \() ->
                    readyToTag
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.clickButton "Save"
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (pictureSaved "blob:https://pixo.pics/1" (stored 1 "blob:https://pixo.pics/2" "a bonsai" 512 512))
                        |> ProgramTest.ensureViewHas [ attribute (Html.Attributes.attribute "data-tooltip" "Tagging a picture") ]
                        |> ProgramTest.ensureViewHas
                            [ attribute (Html.Attributes.attribute "role" "status"), containing [ text "Tagging a picture" ] ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "TaggerLoaded")
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (Encode.object [ ( "tag", Encode.string "TagsWritten" ), ( "text", Encode.string "bonsai, tree, studio" ) ])
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "TaggerReleased")
                        |> ProgramTest.expectViewHasNot [ attribute (Html.Attributes.attribute "data-tooltip" "Tagging a picture") ]
            , test "says on the logo that it is generating" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (generationStep 0)
                        |> ProgramTest.expectViewHas [ attribute (Html.Attributes.attribute "data-tooltip" "Generating a picture") ]
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
                        |> ProgramTest.simulateIncomingPort "interopToElm" olderPictures
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            outgoing
                            (List.filter (Tuple.first >> (==) "LoadTagger") >> Expect.equal [])
                        |> ProgramTest.clickLink "Settings" "/settings/models"
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
                        |> ProgramTest.simulateIncomingPort "interopToElm" olderPictures
                        |> ProgramTest.clickLink "Settings" "/settings/models"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (List.member "LoadTagger" >> Expect.equal False)
            , test "keeps the prompt writer while it tags" <|
                \() ->
                    readyToTag
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.clickButton "Save"
                        |> ProgramTest.simulateIncomingPort "interopToElm"
                            (pictureSaved "blob:https://pixo.pics/1" (stored 1 "blob:https://pixo.pics/2" "a bonsai" 512 512))
                        |> ProgramTest.clickLink "Settings" "/settings/models"
                        |> ProgramTest.clickButton "Remove the prompt writer"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (List.member "RemoveStylist" >> Expect.equal False)
                        |> ProgramTest.expectViewHasNot [ text "Prompt model removed." ]
            , test "opens a new picture bigger, with everything that made it" <|
                \() ->
                    ready
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.ensureViewHas
                            [ tag "figcaption", containing [ text "Bonsai ternary, 2-bit" ] ]
                        |> ProgramTest.ensureViewHasNot [ tag "elmcn-dialog", attribute (Html.Attributes.attribute "open" "") ]
                        |> ProgramTest.clickButton "Open a bonsai"
                        |> ProgramTest.ensureViewHas [ tag "elmcn-dialog", attribute (Html.Attributes.attribute "open" "") ]
                        |> ProgramTest.ensureViewHas
                            [ id "studio-picture", containing [ text "512 × 512 · 4 steps · seed " ], containing [ text "Bonsai ternary, 2-bit" ] ]
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
                        |> ProgramTest.fillIn "prompt" "Prompt" "a bonsai"
                        |> ProgramTest.clickButton "Generate"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (imageGenerated "blob:https://pixo.pics/1")
                        |> ProgramTest.clickButton "Save"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (libraryFailed "The quota has been exceeded.")
                        |> ProgramTest.ensureViewHasNot [ text "Saving…" ]
                        |> ProgramTest.clickButton "Save"
                        |> ProgramTest.expectViewHas [ text "Saving…" ]
            ]
        , describe "AI Style"
            [ test "loads no prompt model on a browser that can't run it" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked False)
                        |> ProgramTest.update (Main.GotStylist (Stylist.StyleChosen Stylist.Photo "a fox in the snow"))
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (Expect.equal [ "CheckWebGpu" ])
            , test "loads no prompt model on a GPU without 16-bit shaders" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (adapterFound [])
                        |> ProgramTest.update (Main.GotStylist (Stylist.StyleChosen Stylist.Photo "a fox in the snow"))
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
                        |> ProgramTest.ensureViewHas [ text "Describe the picture first." ]
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
                        |> ProgramTest.ensureViewHas [ text "Loading the prompt model" ]
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
                        |> ProgramTest.ensureViewHas [ text "Writing a Photo prompt…" ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (promptStreamed "<think>\n\n</think>\n\nSubject: a red fox curled up in fresh snow\nSty")
                        |> ProgramTest.ensureViewHas [ id "prompt", attribute (Html.Attributes.value "Subject: a red fox curled up in fresh snow"), attribute (Html.Attributes.readonly True) ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (promptWritten ("<think>\n\n</think>\n\n" ++ foxPrompt))
                        |> ProgramTest.ensureViewHas [ id "prompt", attribute (Html.Attributes.value foxPrompt) ]
                        |> ProgramTest.expectViewHas [ tag "button", containing [ text "Undo" ] ]
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
                        |> ProgramTest.ensureViewHasNot [ text "Undo" ]
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
                        |> ProgramTest.ensureViewHasNot [ text "Undo" ]
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
                        |> ProgramTest.clickButton "Stop"
                        |> ProgramTest.ensureOutgoingPortValues "interopFromElm"
                            (Decode.field "tag" Decode.string)
                            (List.reverse >> List.head >> Expect.equal (Just "StopWriting"))
                        |> ProgramTest.ensureViewHas [ text "Stopping…" ]
                        |> ProgramTest.simulateIncomingPort "interopToElm" (promptWritten "Subject: a red fox")
                        |> ProgramTest.ensureViewHasNot [ text "Undo" ]
                        |> ProgramTest.expectViewHas [ id "prompt", attribute (Html.Attributes.value "a fox in the snow") ]
            , test "keeps the idea when the answer is not a prompt" <|
                \() ->
                    start defaultFlags
                        |> ProgramTest.simulateIncomingPort "interopToElm" (webGpuChecked True)
                        |> ProgramTest.fillIn "prompt" "Prompt" "a fox in the snow"
                        |> ProgramTest.clickButton "Photo"
                        |> ProgramTest.simulateIncomingPort "interopToElm" (tagged "StylistLoaded")
                        |> ProgramTest.simulateIncomingPort "interopToElm" (promptWritten "\"Cozy\"")
                        |> ProgramTest.ensureViewHas [ text "That didn't come out right. Try again." ]
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
        , describe "About"
            [ test "checks the server as it opens, with its release, region and latency" <|
                \() ->
                    startAt "/settings/about" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET"
                            "/api/health"
                            """{"status":"pass","releaseId":"7990ba2c0ffee","region":"fra1","links":{"release":"https://github.com/ricardocasares/pixo.pics/commit/7990ba2c0ffee"},"checks":{"database":{"status":"pass","latencyMs":25}}}"""
                        |> ProgramTest.ensureViewHas [ text "fra1" ]
                        |> ProgramTest.ensureViewHas [ text "25 ms" ]
                        |> ProgramTest.expectViewHas
                            [ tag "a"
                            , attribute (Html.Attributes.href "https://github.com/ricardocasares/pixo.pics/commit/7990ba2c0ffee")
                            , containing [ text "7990ba2" ]
                            ]
            , test "says that prompts and pictures never reach the server" <|
                \() ->
                    startAt "/settings/about" defaultFlags
                        |> ProgramTest.ensureViewHas [ text "Your prompts and pictures stay on this device." ]
                        |> ProgramTest.expectViewHas [ text "The server is only used for authentication: your account, passkeys and sessions." ]
            , test "checks again each time it is opened" <|
                \() ->
                    startAt "/settings/about" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET" "/api/health" health
                        |> ProgramTest.clickLink "Models" "/settings/models"
                        |> ProgramTest.clickLink "About" "/settings/about"
                        |> ProgramTest.expectHttpRequestWasMade "GET" "/api/health"
            , test "says why the database is slow" <|
                \() ->
                    startAt "/settings/about" defaultFlags
                        |> ProgramTest.simulateHttpOk "GET"
                            "/api/health"
                            """{"status":"warn","region":"fra1","checks":{"database":{"status":"warn","latencyMs":1500,"output":"Slower than 1000 ms."}}}"""
                        |> ProgramTest.ensureViewHas [ text "1500 ms" ]
                        |> ProgramTest.expectViewHas [ text "Slower than 1000 ms." ]
            , test "reads a failed check from its 503" <|
                \() ->
                    startAt "/settings/about" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET"
                            "/api/health"
                            (response 503 """{"status":"fail","checks":{"database":{"status":"fail","latencyMs":5000,"output":"Timed out after 5000 ms."}}}""")
                        |> ProgramTest.expectViewHas [ text "Timed out after 5000 ms." ]
            , test "says when the server can't be reached" <|
                \() ->
                    startAt "/settings/about" defaultFlags
                        |> ProgramTest.simulateHttpResponse "GET" "/api/health" Test.Http.networkError
                        |> ProgramTest.expectViewHas [ text "Could not reach the server. Start both apps with `bun dev`." ]
            , test "is listed in the settings" <|
                \() ->
                    startAt "/settings/models" defaultFlags
                        |> ProgramTest.clickLink "About" "/settings/about"
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
                        |> ProgramTest.ensureViewHas [ text "Prompt model removed." ]
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
                        |> ProgramTest.ensureViewHas [ text "Loading the transformer" ]
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
                        |> ProgramTest.ensureViewHas [ text "In memory" ]
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.maybe localStorageWrite)
                            (List.filterMap (Maybe.map .value)
                                >> Expect.equal [ """["prism-ml/bonsai-image-ternary-4B-mlx-2bit"]""" ]
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
                        |> ProgramTest.ensureViewHas [ text "Downloaded models removed." ]
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
                        |> ProgramTest.ensureViewHas [ text "Downloaded models removed." ]
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
        , describe "Library"
            [ test "asks the browser for the saved pictures" <|
                \() ->
                    startAt "/library" defaultFlags
                        |> ProgramTest.expectOutgoingPortValues "interopFromElm"
                            (Decode.map (Encode.encode 0) Decode.value)
                            (Expect.equal
                                [ "{\"tag\":\"IdbGetAll\",\"data\":{\"database\":{\"name\":\"pixo\",\"version\":1,\"stores\":[{\"name\":\"pictures\",\"keyPath\":\"id\",\"autoIncrement\":true}]},\"store\":\"pictures\",\"label\":\"\"}}" ]
                            )
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
            , test "shows only the pictures with the tag chosen" <|
                \() ->
                    taggedLibrary
                        |> ProgramTest.clickButton "sunset"
                        |> ProgramTest.ensureViewHas [ text "2 of 3 pictures" ]
                        |> ProgramTest.ensureViewHasNot [ attribute (Html.Attributes.attribute "aria-label" "Open a red fox in the snow") ]
                        |> ProgramTest.clickButton "All pictures"
                        |> ProgramTest.expectViewHas [ text "3 pictures" ]
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
        |> ProgramTest.clickLink "Settings" "/settings/models"
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
        |> ProgramTest.clickLink "Settings" "/settings/models"
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


taggedLibrary : ProgramTest Main.Model Main.Msg (Effect Main.Msg)
taggedLibrary =
    startAt "/library" defaultFlags
        |> ProgramTest.simulateIncomingPort "interopToElm"
            (libraryLoaded
                [ storedWithTags [ "fox" ] 1 "blob:https://pixo.pics/1" "a red fox in the snow" 512 512
                , storedWithTags [ "sunset", "lighthouse" ] 2 "blob:https://pixo.pics/2" "a lighthouse on a cliff at sunset" 1024 576
                , storedWithTags [ "sunset" ] 3 "blob:https://pixo.pics/3" "a city skyline at sunset" 576 1024
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
stored id url prompt width height =
    Encode.object
        [ ( "id", Encode.int id )
        , ( "blob", Encode.string url )
        , ( "prompt", Encode.string prompt )
        , ( "width", Encode.int width )
        , ( "height", Encode.int height )
        , ( "steps", Encode.int 4 )
        , ( "seed", Encode.int 7 )
        , ( "model", Encode.string "prism-ml/bonsai-image-ternary-4B-mlx-2bit" )
        ]


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
