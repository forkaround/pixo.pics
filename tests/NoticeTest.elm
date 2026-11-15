module NoticeTest exposing (suite)

import ApiClient
import Expect
import Notice exposing (Notice)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (containing, tag, text)
import UI.Toast as Toast


suite : Test
suite =
    describe "Notice"
        [ describe "failure"
            [ test "waits and tries again after a 429" <|
                \() ->
                    Notice.failure { id = "a", title = "T", retry = Just "again" } (ApiClient.Throttled 30 "x")
                        |> Expect.all
                            [ .retry >> Expect.equal (Notice.RetryIn 30 "again")
                            , .description >> Expect.equal (Just "Too many attempts. Trying again in 30 seconds.")
                            ]
            , test "offers to try again after a failure" <|
                \() ->
                    Notice.failure { id = "a", title = "T", retry = Just "again" } (ApiClient.Failed "Network offline")
                        |> Expect.all
                            [ .retry >> Expect.equal (Notice.Retry "again")
                            , .description >> Expect.equal (Just "Network offline")
                            ]
            , test "offers nothing after a refusal" <|
                \() ->
                    Notice.failure { id = "a", title = "T", retry = Just "again" } (ApiClient.Refused "Taken.")
                        |> Expect.all
                            [ .retry >> Expect.equal Notice.NoRetry
                            , .description >> Expect.equal (Just "Taken.")
                            ]
            , test "offers nothing when there is nothing to retry" <|
                \() ->
                    Notice.failure { id = "a", title = "T", retry = Nothing } (ApiClient.Throttled 30 "Too many attempts, try again later")
                        |> Expect.all
                            [ .retry >> Expect.equal Notice.NoRetry
                            , .description >> Expect.equal (Just "Too many attempts, try again later")
                            ]
            ]
        , describe "wait"
            [ test "says seconds, minutes and hours" <|
                \() ->
                    List.map Notice.wait [ 0, 1, 30, 60, 61, 900, 3600, 5400 ]
                        |> Expect.equal
                            [ "1 second"
                            , "1 second"
                            , "30 seconds"
                            , "1 minute"
                            , "2 minutes"
                            , "15 minutes"
                            , "1 hour"
                            , "2 hours"
                            ]
            ]
        , describe "map"
            [ test "wraps the message of a retry" <|
                \() ->
                    Notice.map String.fromInt (notice (Notice.Retry 5))
                        |> .retry
                        |> Expect.equal (Notice.Retry "5")
            , test "wraps the message of a timed retry" <|
                \() ->
                    Notice.map String.fromInt (notice (Notice.RetryIn 3 5))
                        |> .retry
                        |> Expect.equal (Notice.RetryIn 3 "5")
            ]
        , describe "toToast"
            [ test "offers a button when a retry is up to the reader" <|
                \() ->
                    rendered (notice (Notice.Retry Retried))
                        |> Query.findAll [ tag "button", containing [ text "Try again" ] ]
                        |> Query.count (Expect.equal 1)
            , test "offers no button when the retry is automatic" <|
                \() ->
                    rendered (notice (Notice.RetryIn 3 Retried))
                        |> Query.findAll [ tag "button", containing [ text "Try again" ] ]
                        |> Query.count (Expect.equal 0)
            , test "offers no button when there is no retry" <|
                \() ->
                    rendered (notice Notice.NoRetry)
                        |> Query.findAll [ tag "button", containing [ text "Try again" ] ]
                        |> Query.count (Expect.equal 0)
            ]
        ]


notice : Notice.Retry msg -> Notice msg
notice retry =
    { id = "a", tone = Notice.Problem, title = "T", description = Just "D", retry = retry }


type Msg
    = Toasted (Toast.Msg Msg)
    | Retried


rendered : Notice Msg -> Query.Single Msg
rendered value =
    Toast.update Toasted (Toast.add (Notice.toToast value)) Toast.init
        |> Tuple.first
        |> Toast.view Toasted
        |> Query.fromHtml
