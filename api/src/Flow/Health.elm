module Flow.Health exposing (Msg, check, update)

import Api
import Db
import Effect exposing (Effect(..), log, respond)
import InteropDefinitions as Interop
import RateLimit
import Reply exposing (Reply(..))
import Request exposing (Request)
import Response
import Server exposing (Server)
import Services.Neon as Neon


type Msg
    = Checked Interop.Resolver ( Int, Result Neon.Error RateLimit.Outcome )


check : Server -> Request -> Reply Msg
check server request =
    Later
        (\resolver ->
            Database
                (Neon.commit (Neon.withTimeout healthTimeout server.db)
                    (Neon.transaction always
                        |> Neon.with
                            (Db.rateLimit (RateLimit.healthChecks (Request.clientAddress request)))
                        |> Neon.with Db.ping
                    )
                    (\result latency -> Ok (Checked resolver ( latency, result )))
                )
        )


update : Server -> Msg -> Effect Msg
update server msg =
    case msg of
        Checked resolver ( _, Ok (RateLimit.Limited limited) ) ->
            respond resolver (Response.tooManyRequests limited.retryAfter)

        Checked resolver ( latency, Ok (RateLimit.Allowed _) ) ->
            if latency > slowAfter then
                Batch
                    [ respond resolver
                        (Response.health server.release
                            { status = Api.Warn
                            , latencyMs = Just latency
                            , output = Just ("Slower than " ++ String.fromInt slowAfter ++ " ms.")
                            }
                        )
                    , log Interop.Warn ("Health check slow: the database took " ++ String.fromInt latency ++ " ms.")
                    ]

            else
                respond resolver
                    (Response.health server.release { status = Api.Pass, latencyMs = Just latency, output = Nothing })

        Checked resolver ( latency, Err problem ) ->
            Batch
                [ respond resolver
                    (Response.health server.release { status = Api.Fail, latencyMs = Just latency, output = Just (failure problem) })
                , log Interop.Error ("Health check failed: " ++ Neon.errorToString problem)
                ]


healthTimeout : Float
healthTimeout =
    5000


slowAfter : Int
slowAfter =
    1000


failure : Neon.Error -> String
failure problem =
    case problem of
        Neon.Timeout ->
            "Timed out after " ++ String.fromFloat healthTimeout ++ " ms."

        Neon.NetworkError ->
            "Unreachable."

        Neon.BadUrl _ ->
            "Unreachable."

        Neon.ServerError status _ ->
            "Neon answered with HTTP " ++ String.fromInt status ++ "."

        Neon.QueryFailed _ ->
            "The query failed."

        Neon.UnexpectedResult _ ->
            "Unexpected result."
