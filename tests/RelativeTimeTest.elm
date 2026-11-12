module RelativeTimeTest exposing (suite)

import Expect
import RelativeTime
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "RelativeTime"
        (List.map
            (\( seconds, expected ) ->
                test (String.fromInt seconds ++ " seconds") <|
                    \() ->
                        RelativeTime.toString (Time.millisToPosix (seconds * 1000)) "1970-01-01T00:00:00Z"
                            |> Expect.equal expected
            )
            [ ( 0, "just now" )
            , ( 59, "just now" )
            , ( 60, "1 minute ago" )
            , ( 120, "2 minutes ago" )
            , ( 3600, "1 hour ago" )
            , ( 86400, "1 day ago" )
            , ( 172800, "2 days ago" )
            , ( 2592000, "1 month ago" )
            , ( 31536000, "1 year ago" )
            , ( -3600, "in 1 hour" )
            ]
            ++ [ test "handles timezone offsets" <|
                    \() ->
                        RelativeTime.toString (Time.millisToPosix 3600000) "1970-01-01T02:00:00+02:00"
                            |> Expect.equal "1 hour ago"
               , test "handles invalid timestamps" <|
                    \() ->
                        RelativeTime.toString (Time.millisToPosix 0) "invalid"
                            |> Expect.equal "Unknown date"
               ]
        )
