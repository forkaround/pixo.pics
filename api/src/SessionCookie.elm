module SessionCookie exposing (clear, lifetimeSeconds, read, set)

import Data.Origin as Origin exposing (Origin)
import Data.SessionToken as SessionToken exposing (SessionToken)
import Request exposing (Request)
import Response exposing (Response)


name : String
name =
    "session"


lifetimeSeconds : Int
lifetimeSeconds =
    30 * 24 * 60 * 60


read : Request -> Maybe SessionToken
read request =
    Request.header "cookie" request
        |> Maybe.withDefault ""
        |> String.split ";"
        |> List.map String.trim
        |> List.filter (String.startsWith (name ++ "="))
        |> List.head
        |> Maybe.map (String.dropLeft (String.length name + 1))
        |> Maybe.andThen SessionToken.fromString


set : Origin -> SessionToken -> Response -> Response
set origin token =
    Response.withHeader "set-cookie" (value origin (SessionToken.toString token) lifetimeSeconds)


clear : Origin -> Response -> Response
clear origin =
    Response.withHeader "set-cookie" (value origin "" 0)


value : Origin -> String -> Int -> String
value origin token maxAge =
    String.join "; "
        (List.concat
            [ [ name ++ "=" ++ token
              , "Path=/"
              , "Max-Age=" ++ String.fromInt maxAge
              , "HttpOnly"
              , "SameSite=Lax"
              ]
            , if Origin.isSecure origin then
                [ "Secure" ]

              else
                []
            ]
        )
