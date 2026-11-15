module Notice exposing
    ( Notice
    , Retry(..)
    , Tone(..)
    , failure
    , map
    , toToast
    , wait
    )

import ApiClient
import UI.Toast as Toast


type alias Notice msg =
    { id : String
    , tone : Tone
    , title : String
    , description : Maybe String
    , retry : Retry msg
    }


type Tone
    = Problem
    | Success
    | Information


type Retry msg
    = NoRetry
    | Retry msg
    | RetryIn Int msg


failure : { id : String, title : String, retry : Maybe msg } -> ApiClient.Error -> Notice msg
failure { id, title, retry } error =
    let
        problem description again =
            { id = id, tone = Problem, title = title, description = Just description, retry = again }
    in
    case ( error, retry ) of
        ( ApiClient.Throttled seconds _, Just again ) ->
            problem ("Retrying in " ++ wait seconds ++ ".") (RetryIn seconds again)

        ( ApiClient.Failed reason, Just again ) ->
            problem reason (Retry again)

        _ ->
            problem (ApiClient.errorToString error) NoRetry


wait : Int -> String
wait seconds =
    if seconds < 60 then
        count (max 1 seconds) "second"

    else if seconds < 3600 then
        count ((seconds + 59) // 60) "minute"

    else
        count ((seconds + 3599) // 3600) "hour"


count : Int -> String -> String
count amount unit =
    String.fromInt amount
        ++ " "
        ++ unit
        ++ (if amount == 1 then
                ""

            else
                "s"
           )


map : (a -> b) -> Notice a -> Notice b
map toMsg notice =
    { id = notice.id
    , tone = notice.tone
    , title = notice.title
    , description = notice.description
    , retry =
        case notice.retry of
            NoRetry ->
                NoRetry

            Retry msg ->
                Retry (toMsg msg)

            RetryIn seconds msg ->
                RetryIn seconds (toMsg msg)
    }


toToast : Notice msg -> Toast.Toast Toast.Ready msg
toToast notice =
    Toast.new
        |> Toast.title notice.title
        |> Toast.id notice.id
        |> withDescription notice.description
        |> withTone notice.tone
        |> withRetry notice.retry


withDescription : Maybe String -> Toast.Toast Toast.Ready msg -> Toast.Toast Toast.Ready msg
withDescription description toast =
    case description of
        Just text ->
            Toast.description text toast

        Nothing ->
            toast


withTone : Tone -> Toast.Toast status msg -> Toast.Toast status msg
withTone tone =
    case tone of
        Problem ->
            Toast.error >> Toast.highPriority

        Success ->
            Toast.success

        Information ->
            Toast.info


withRetry : Retry msg -> Toast.Toast status msg -> Toast.Toast status msg
withRetry retry =
    case retry of
        NoRetry ->
            identity

        Retry msg ->
            Toast.action "Try again" msg >> Toast.timeout 0

        RetryIn seconds _ ->
            Toast.warning >> Toast.timeout (toFloat (seconds * 1000))
