module RelativeTime exposing (toString)

import Iso8601
import Time exposing (Posix)


toString : Posix -> String -> String
toString now value =
    case Iso8601.toTime value of
        Err _ ->
            "Unknown date"

        Ok date ->
            let
                seconds =
                    (Time.posixToMillis date - Time.posixToMillis now) // 1000

                units =
                    [ ( "year", 31536000 )
                    , ( "month", 2592000 )
                    , ( "day", 86400 )
                    , ( "hour", 3600 )
                    , ( "minute", 60 )
                    ]
            in
            case List.filter (\( _, duration ) -> abs seconds >= duration) units |> List.head of
                Nothing ->
                    "just now"

                Just ( unit, duration ) ->
                    let
                        count =
                            abs seconds // duration

                        amount =
                            String.fromInt count
                                ++ " "
                                ++ unit
                                ++ (if count == 1 then
                                        ""

                                    else
                                        "s"
                                   )
                    in
                    if seconds < 0 then
                        amount ++ " ago"

                    else
                        "in " ++ amount
